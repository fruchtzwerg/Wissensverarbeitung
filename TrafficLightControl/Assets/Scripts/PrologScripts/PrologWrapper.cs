using System;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityThreading;

public class PrologWrapper : MonoBehaviour
{

    public string[] PrologFiles;
    public string AStarPl;
    private Job _jobA;
    private Job _jobB;
    private Job _jobSpawn;

    public UnityLogger LoggerA;
    public ConsoleView ConsoleView_A;
    public ConsoleView ConsoleView_B;
    public ConsoleView ConsoleView_Spawn;
    public AStar aStar;

    private string aStarTree;
    private ActionThread threadA;
    private ActionThread threadB;
    private ActionThread threadSpawn;
    private UnityLogger LoggerB;
    private UnityLogger LoggerSpawn;


    //Use this for initialization
    void Start()
    {
        if (LoggerA == null)
            LoggerA = new UnityLogger(ConsoleView_A);
        if (LoggerB == null)
            LoggerB = new UnityLogger(ConsoleView_B);
        if (LoggerSpawn == null)
            LoggerSpawn = new UnityLogger(ConsoleView_Spawn);

        aStarTree = aStar.BuildAStarTreeString();

        threadA = UnityThreadHelper.CreateThread(() => DoWorkA());
        threadB = UnityThreadHelper.CreateThread(() => DoWorkB());
        threadSpawn = UnityThreadHelper.CreateThread(() => DoWorkSpawn());
    }


    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    private void DoWorkA()
    {
        print("Launching SWI-Prolog A...");

        // start create job object
        _jobA = new Job(LoggerA);
        JobStartedA();
    }
    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    private void DoWorkB()
    {
        print("Launching SWI-Prolog B...");

        // start create job object
        _jobB = new Job(LoggerB);
        JobStartedB();
    }
    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    private void DoWorkSpawn()
    {
        print("Launching SWI-Prolog Spawn...");

        // start create job object
        _jobSpawn = new Job(LoggerSpawn);
        JobStartedSpawn();
    }


    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    private void JobStartedA( )
    {
        // swi-prolog is running
        print("SWI-Prolog A is running...");

        //_prolog.Query("protocol('prolog.log').");

        //consult all given prolog knowledge base files
        foreach (var pl in PrologFiles)
        {
            _jobA.ConsultFile(pl);
        }
    }


    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    private void JobStartedB()
    {
        // swi-prolog is running
        print("SWI-Prolog B is running...");

        //_prolog.Query("protocol('prolog.log').");

        //consult all given prolog knowledge base files
        foreach (var pl in PrologFiles)
        {
            _jobB.ConsultFile(pl);
        }
    }


    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    private void JobStartedSpawn()
    {
        // swi-prolog is running
        print("SWI-Prolog Spawn is running...");

        //_prolog.Query("protocol('prolog.log').");

        //consult all given prolog knowledge base files
        _jobSpawn.ConsultFile(AStarPl);

        var query = "setNewGraph(" + aStarTree + ").";
        _jobSpawn.Query(query);
    }


    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit()
    {
        //_prolog.Query("noprotocol.");
        threadA.Exit();
        if (_jobA != null)
            _jobA.Kill();
        threadB.Exit();
        if (_jobB != null)
            _jobB.Kill();
        threadSpawn.Exit();
        if (_jobSpawn != null)
            _jobSpawn.Kill();

        LoggerA.Timer.Dispose();
        LoggerB.Timer.Dispose();
        LoggerSpawn.Timer.Dispose();
    }


    /// <summary>
    /// Query prolog with response
    /// </summary>
    /// <param name="query">Query string</param>
    /// <param name="sender">Sender Object</param>
    /// <param name="xRoad">Crossroad</param>
    public void QueryProlog(string query, IProlog sender = null, string xRoad = "")
    {
        Job job;

        switch (xRoad)
        {
            case "a":
                job = _jobA;
                break;
            case "b":
                job = _jobB;
                break;
            default:
                job = _jobSpawn;
                break;
        }

        if (string.IsNullOrEmpty(query)) return;
        job.Query(query, sender);
        Log(UnityLogger.DELIMITER_SEND + query, xRoad);
    }



    public void Log(string message, string xRoad = "")
    {
        UnityLogger logger;

        switch (xRoad)
        {
            case "a":
                logger = LoggerA;
                break;
            case "b":
                logger = LoggerB;
                break;
            default:
                logger = LoggerSpawn;
                break;
        }

        logger.Log(message);
    }


    //################################################################\\
    //#######################  static functions  #####################\\
    //################################################################\\

    /// <summary>
    /// Parses a prolog string into a SequenceInfo object.
    /// SequenceInfo contains: array of Lights
    ///                     current sequence
    ///                     duration of timer
    /// </summary>
    /// <param name="data">prolog string</param>
    /// <returns></returns>
    public static SequenceInfo ParseSequenceInfo(string data)
    {
        var index = data.IndexOf('[');
        if (index < 0)
        {
            throw new ArgumentOutOfRangeException("prolog answer", data);
        }

        var info = new SequenceInfo();

        var tmp = data.Substring(index);
        var regex = new Regex(@"([^\,\s\[\]\.]+)");
        var matches = regex.Matches(tmp);

        info.GreenLightes = new TrafficLight.Lights[matches.Count - 2];

        for (var i = 0; i < matches.Count - 2; i++)
        {
            info.GreenLightes[i] = ParseEnum<TrafficLight.Lights>(matches[i].Groups[0].Value);
        }

        info.Sequence = ParseEnum<SequenceInfo.JunctionSequence>(matches[matches.Count - 2].Groups[0].Value);
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
        var q = "path(" + origin.ToLower() + "," + destination.ToLower() + ",Path).";
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
        NextSequence,
        Event
    }


    /// <summary>
    /// Builds a query string for prolog from arguments.
    /// </summary>
    /// <param name="type">type of query</param>
    /// <param name="xRoad">crossroad</param>
    /// <param name="sequence">current sequence</param>
    /// <param name="args">argument (return value)</param>
    /// <returns></returns>
    public static string BuildQuery(QueryType type,
        TrafficLightControl.Crossroads xRoad,
        SequenceInfo.JunctionSequence sequence,
        params string[] args
        )
    {
        switch (type)
        {
            case QueryType.NextSequence:
                if (args.Length < 1)
                    return null;

                return "nextSequence(" + xRoad + ", " + args[0] + ", " + sequence + ").";
            case QueryType.Event:
                if (args.Length < 1)
                    return null;

                return "event(" + xRoad + ", " + args[0] + ", " + sequence + ").";
            default:
                throw new ArgumentOutOfRangeException("type", type, null);
        }
    }
}