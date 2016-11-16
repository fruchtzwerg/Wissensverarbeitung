using System.ComponentModel;
using UnityEngine;

public class PrologWrapper : MonoBehaviour
{

    public string[] PrologFiles;
    private Job _prolog;

    public UnityLogger unityLogger;

    //Use this for initialization
    void Start()
    {
        // create new backround worker
        var worker = new BackgroundWorker();
        // register listeners
        worker.DoWork += DoWork;
        worker.RunWorkerCompleted += RunWorkerInitCompleted;

        // execute worker asynchronously
        worker.RunWorkerAsync();
    }

    /// <summary>
    /// Executed on Main-Thread when worker is done.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void RunWorkerInitCompleted(object sender, RunWorkerCompletedEventArgs args)
    {
        // swi-prolog is running
        print("SWI-Prolog is running...");        
        
        //consult all given prolog knowledge base files
        for (int i = 0; i < PrologFiles.Length; i++)
            _prolog.ConsultFile(PrologFiles[i]);
    }

    /// <summary>
    /// Executed asynchronously during run of worker.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    private void DoWork(object sender, DoWorkEventArgs args)
    {        
        print("Launching SWI-Prolog...");

         // start create job object
         _prolog = new Job();

         // run swi-prolog inside cmd
         _prolog.initPrologProcess(unityLogger);
        
        // query for something
        //_prolog.Query("X is 2+6.");
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

    /// <summary>
    /// Query prolog without respons
    /// </summary>
    /// <param name="query"></param>
    public void QueryProlog(string query) {
        if(!string.IsNullOrEmpty(query))
            _prolog.Query(query);
    }

    /// <summary>
    /// Query prolog with respons
    /// </summary>
    /// <param name="query">Query string</param>
    /// <param name="sender">Sender Object</param>
    public void QueryProlog(string query, IProlog sender) {
        if (sender == null)
            return;

        _prolog.Query(query, sender);
    }
}
