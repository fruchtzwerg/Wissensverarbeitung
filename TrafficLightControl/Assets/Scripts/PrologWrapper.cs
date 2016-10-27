

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Threading;
using UnityEngine;

public class PrologWrapper : MonoBehaviour
{

    public string[] PrologFiles;
    private Job _prolog;

    //Use this for initialization
    void Start()
    {
        //for (int i = 0; i < PrologFiles.Length; i++)
        //    _prolog.consultFile(PrologFiles[i]);
        
        // create new backround worker
        var worker = new BackgroundWorker();
        // register listeners
        worker.DoWork += Query;
        worker.RunWorkerCompleted += HandleResults;
        // execute worker asynchronously
        worker.RunWorkerAsync();
    }

    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void HandleResults(object sender, RunWorkerCompletedEventArgs args)
    {
        // swi-prolog is running
        print("SWI-Prolog is running...");
    }

    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void Query(object sender, DoWorkEventArgs args)
    {
        print("Launching SWI-Prolog...");

        // start create job object
        _prolog = new Job();

        // run swi-prolog inside cmd
        _prolog.initPrologProcess();

        // query for something
        _prolog.Query("X is 2+6.");
    }



    //Update is called once per frame
    void Update()
    {

    }

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit()
    {
        _prolog.Kill();
    }
}
