using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityThreading;
using Object = UnityEngine.Object;

public class PrologWrapper : MonoBehaviour
{

    public string[] PrologFiles;
    private Job _prolog;

    public UnityLogger UnityLogger;
    public AStar aStar;

    private string aStarTree;
    private ActionThread thread;

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
    private void RunWorkerInitCompleted(/*object sender, RunWorkerCompletedEventArgs args*/)
    {
        // swi-prolog is running
        print("SWI-Prolog is running...");        
        
        //consult all given prolog knowledge base files
        foreach (var pl in PrologFiles)
            _prolog.ConsultFile(pl);

        var query = "setNewGraph(" + aStarTree + ").";
        _prolog.Query(query); ;
    }


    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void DoWork(/*object sender, DoWorkEventArgs args*/)
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
        if(_prolog != null)
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






    public static string GetPath(string origin, string destination)
    {
        var q = "getPath(" + origin.ToLower() + "," + destination.ToLower() + ",Path).";
        return q;
    }

    public static SplineWaypoint[] ParseArray(string data)
    {
        var index = data.IndexOf('[');
        if (index < 0)
        {
            return null;
        }

        var tmp = data.Substring(data.IndexOf('['));
        var regex = new Regex(@"([^\,\s\[\]\.]+)");
        var matches = regex.Matches(tmp);
        var globalWaypoints = FindObjectsOfType<SplineWaypoint>().ToList();

        SplineWaypoint[] ary = new SplineWaypoint[matches.Count];
        for (int i = 0; i < matches.Count; i++)
        {
            // for match
            var match = matches[i];
            if (match.Success)  // if match was successfull
            {
                // map string to waypoint
                ary[i] = globalWaypoints.Find(wp =>  // in all waypoints in the scene find waypoints that...
                wp.name.Equals(match.Groups[0].Value, StringComparison.OrdinalIgnoreCase) // wp.name = match.name
                &&  // AND
                (wp.IsDestination   // waypoint is either a destination (has no followers)
                ||  // OR           wp.next.name = previousMatch.name (matches are inversed -> next waypoint is i-1)
                (wp.NextWaypoint.name.ToLower() == matches[i-1].Groups[0].Value)));
            }
        }

        //var ary = (from Match match in matches
        //           where match.Success
        //           select ts.Find(t => t.name.Equals(match.Groups[0].Value, StringComparison.OrdinalIgnoreCase))).ToArray();
        
        return ary;
    }
}
