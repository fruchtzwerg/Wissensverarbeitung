using System;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityThreading;

public class PrologWrapper : MonoBehaviour, IProlog
{

    public string[] PrologFiles;
    private Job _prolog;

    public UnityLogger UnityLogger;
    public AStar aStar;

    private string aStarTree;
    private ActionThread thread;

    public static PhaseInfo.JunctionPhase PhaseA;
    public static PhaseInfo.JunctionPhase PhaseB;

    //Use this for initialization
    void Start()
    {
        if (UnityLogger == null)
            UnityLogger = GetComponent<UnityLogger>();

        aStarTree = aStar.BuildAStarTreeString();

        //// create new backround worker
        //var worker = new BackgroundWorker();
        //// register listeners
        //worker.DoWork += DoWork;
        //worker.RunWorkerCompleted += RunWorkerInitCompleted;

        //// execute worker asynchronously
        //worker.RunWorkerAsync();

        thread = UnityThreadHelper.CreateThread(() => DoWork());
    }


    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void RunWorkerInitCompleted( /*object sender, RunWorkerCompletedEventArgs args*/)
    {
        // swi-prolog is running
        print("SWI-Prolog is running...");

        //consult all given prolog knowledge base files
        foreach (var pl in PrologFiles)
            _prolog.ConsultFile(pl);

        var query = "setNewGraph(" + aStarTree + ").";
        _prolog.Query(query);
    }


    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void DoWork( /*object sender, DoWorkEventArgs args*/)
    {
        print("Launching SWI-Prolog...");

        // start create job object
        _prolog = new Job(UnityLogger);
        RunWorkerInitCompleted();
    }


    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit()
    {
        thread.Exit();
        if (_prolog != null)
            _prolog.Kill();
    }


    /// <summary>
    /// Query prolog with response
    /// </summary>
    /// <param name="query">Query string</param>
    /// <param name="sender">Sender Object</param>
    public void QueryProlog(string query, IProlog sender = null)
    {
        if (string.IsNullOrEmpty(query)) return;
        _prolog.Query(query, sender);
    }

    public void ReceiveDataFromProlog(string data)
    {
        var phase = data.Split('=')[0].Trim();
        var value = data.Split('=')[1].Trim();

        switch (phase)
        {
            case "PhaseA":
                PhaseA = ParseEnum<PhaseInfo.JunctionPhase>(value);
                break;
            case "PhaseB":
                PhaseB = ParseEnum<PhaseInfo.JunctionPhase>(value);
                break;
        }

        print("phase=" + phase + ", PhaseA=" + PhaseA + "PhaseB=" + PhaseB);
    }


    //################################################################\\
    //#######################  static functions  #####################\\
    //################################################################\\

    /// <summary>
    /// Parses a prolog string into a PhaseInfo object.
    /// PhaseInfo contains: array of Lights
    ///                     current phase
    ///                     duration of timer
    /// </summary>
    /// <param name="data">prolog string</param>
    /// <returns></returns>
    public static PhaseInfo ParsePhaseInfo(string data)
    {
        var index = data.IndexOf('[');
        if (index < 0)
            throw new ArgumentOutOfRangeException();

        var info = new PhaseInfo();

        var tmp = data.Substring(index);
        var regex = new Regex(@"([^\,\s\[\]\.]+)");
        var matches = regex.Matches(tmp);

        info.GreenLightes = new TrafficLight.Lights[matches.Count - 2];

        for (var i = 0; i < matches.Count - 2; i++)
        {
            info.GreenLightes[i] = ParseEnum<TrafficLight.Lights>(matches[i].Groups[0].Value);
        }

        info.Phase = ParseEnum<PhaseInfo.JunctionPhase>(matches[matches.Count - 2].Groups[0].Value);
        info.Duration = int.Parse(matches[matches.Count - 1].Groups[0].Value);

        return info;
    }


    /// <summary>
    /// Parses a string into a typed enum value.
    /// </summary>
    /// <typeparam name="T">enum type</typeparam>
    /// <param name="value">string to parse</param>
    /// <param name="ignoreCase">wheter to ignore case of value or not</param>
    /// <returns></returns>
    private static T ParseEnum<T>(string value, bool ignoreCase = true)
    {
        return (T) Enum.Parse(typeof(T), value, ignoreCase);
    }


    
    //TODO: move into BuildQuery()
    public static string GetPath(string origin, string destination)
    {
        var q = "getPath(" + origin.ToLower() + "," + destination.ToLower() + ",Path).";
        return q;
    }


    /// <summary>
    /// Prases an array from prolog into an array of SplineWaypoints.
    /// </summary>
    /// <param name="data">prolog string</param>
    /// <returns></returns>
    public static SplineWaypoint[] ParseAstarWaypoints(string data)
    {
        var index = data.IndexOf('[');
        if (index < 0)
            return null;

        var tmp = data.Substring(index);
        var regex = new Regex(@"([^\,\s\[\]\.]+)");
        var matches = regex.Matches(tmp);
        var globalWaypoints = FindObjectsOfType<SplineWaypoint>().ToList();

        var ary = new SplineWaypoint[matches.Count];
        for (var i = 0; i < matches.Count; i++)
        {
            // for match
            var match = matches[i];
            if (match.Success) // if match was successfull
            {
                // map string to waypoint
                ary[i] = globalWaypoints.Find(wp => // in all waypoints in the scene find waypoints that...
                    wp.name.Equals(match.Groups[0].Value, StringComparison.OrdinalIgnoreCase) // wp.name = match.name
                    && // AND
                    (wp.IsDestination // waypoint is either a destination (has no followers)
                     || // OR           wp.next.name = previousMatch.name (matches are inversed -> next waypoint is i-1)
                     (wp.NextWaypoint.name.ToLower() == matches[i - 1].Groups[0].Value)));
            }
        }

        return ary;
    }


    /// <summary>
    /// Available query types.
    /// </summary>
    public enum QueryType
    {
        NextPhase,
        Event
    }


    /// <summary>
    /// Builds a query string for prolog from arguments.
    /// </summary>
    /// <param name="type">type of query</param>
    /// <param name="xRoad">crossroad</param>
    /// <param name="phase">currentphase</param>
    /// <param name="args">argument (return value)</param>
    /// <returns></returns>
    public static string BuildQuery(QueryType type,
        TrafficLightControl.Crossroads xRoad,
        PhaseInfo.JunctionPhase phase,
        params string[] args
        )
    {
        switch (type)
        {
            case QueryType.NextPhase:
                if (args.Length < 1)
                    return null;

                return "getnextPhase(" + xRoad + ", " + args[0] + ", " + phase + ").";
            case QueryType.Event:
                if (args.Length < 1)
                    return null;

                return "neuesEreignis(" + xRoad + ", " + args[0] + ", " + phase + ").";
            default:
                throw new ArgumentOutOfRangeException("type", type, null);
        }
    }
}