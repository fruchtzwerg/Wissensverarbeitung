using UnityEngine;
using System.Diagnostics;
using System.IO;
using System;
using System.Text;
using System.Collections.Generic;
using Debug = UnityEngine.Debug;

public class Job
{

    private Process prolog;
    private StreamWriter sw;
    public const string DELIMITERSEND = "--> ";
    public const string DELIMITERRECIVE = "<-- ";
    private UnityLogger unityLogger = new UnityLogger();

    private IProlog sender;

    private Queue<WaitingObject> waitingObjects;

    /// <summary>
    /// Init the swi-prolog.exe in a console window
    /// </summary>
    public void initPrologProcess()
    {
        waitingObjects = new Queue<WaitingObject>();

        prolog = new Process();
        prolog.StartInfo.UseShellExecute = false;
        prolog.StartInfo.RedirectStandardOutput = true;
        prolog.StartInfo.RedirectStandardInput = true;
        prolog.StartInfo.CreateNoWindow = true;
        prolog.StartInfo.Arguments = "-q";

        if (File.Exists(@"C:\Program Files\swipl\bin\swipl.exe"))
            prolog.StartInfo.FileName = @"C:\Program Files\swipl\bin\swipl.exe";
        else if (File.Exists(@"C:\Program Files (x86)\swipl\bin\swipl.exe"))
            prolog.StartInfo.FileName = @"C:\Program Files (x86)\swipl\bin\swipl.exe";
        else {
            prolog.StartInfo.FileName = "swipl.exe";
            Debug.Log("SWI not found!");
            return;
        }
           

        // start the process
        prolog.Start();

        //IO: register STDIN of the process as a streamwriter
        sw = new StreamWriter(prolog.StandardInput.BaseStream, Encoding.ASCII);
        sw.AutoFlush = true;

        //make sure Encodings are compatible
        Console.OutputEncoding = Encoding.ASCII;
        Console.InputEncoding = Encoding.ASCII;

        // register the recieved listener for STDOUT of the process
        prolog.OutputDataReceived += (sender, args) => {
            printToUnity(args.Data);
        };

        // start listening on STDOUT
        prolog.BeginOutputReadLine();
    }


    /// <summary>
    /// consult prolog knowledge base file
    /// </summary>
    /// <param name="path"></param>
    public void ConsultFile(string path)
    {
        if (prolog == null || !File.Exists(path))
            return;
        
        string query = "consult('" + path + "').";
        sw.WriteLine(query);
        sw.Flush();
        
        unityLogger.logProlog(DELIMITERSEND + query);
    }

    /// <summary>
    /// Print a string to Unity's console and logfile.
    /// </summary>
    /// <param name="message"></param>
    private void printToUnity(string message)
    {
        // only print sensible messages
        if(string.IsNullOrEmpty(message.Trim()))
            return;

        // print to console and use delimiter
        //print(DELIMITERRECIVE + message);

        unityLogger.logProlog(DELIMITERRECIVE + message);


        if (waitingObjects.Count > 0) {
            ReplySender(message);  
        }
             
    }

    /// <summary>
    /// send reply to sender
    /// </summary>
    /// <param name="message">message to sender as reply</param>
    private void ReplySender(string message) {
        var waitingObject = waitingObjects.Dequeue();

        //if sender is waiting for response
        if(waitingObject != null && waitingObject.Sender != null) {
            //send reply
            waitingObject.Sender.ReciveDataFromProlog(message);
        } 

        //if there a still other querys to prolog...
        if (waitingObjects.Count > 0) {
            //get fist and query prolog
            var next = waitingObjects.Peek();

            sw.WriteLine(next.Query);
            sw.Flush();
            
            unityLogger.logProlog(DELIMITERSEND + next.Query);

            Debug.Log("Process Name: "+ prolog.ProcessName +", Exit: " +prolog.HasExited);
        }

        //print("Queue Count 2: " + waitingObjects.Count);
    }

    /// <summary>
    /// Query swi-prolog for something.
    /// </summary>
    /// <param name="message">The query string.</param>
    public void Query(string message)
    {
        this.Query(message, null);
    }


    /// <summary>
    /// Query swi-prolog for something.
    /// </summary>
    /// <param name="message">The query string.</param>
    /// <param name="sender">The sender object</param>
    public void Query(string message, IProlog sender) {
        // std-in of prolog still registered?
        // OR: message not null or empty?
        if (sw == null || string.IsNullOrEmpty(message.Trim()))
            return;

        // make sure the query string ends with a '.'
        if (!message.Trim().EndsWith("."))
            message = message + ".";

        //set sender for reply
        waitingObjects.Enqueue(new WaitingObject(message, sender));
        //this.sender = sender;

        //query prolog, if the is no other query
        if(waitingObjects.Count == 1) {
            // write the query to prolog console and execute
            sw.WriteLine(message);
            sw.Flush();

            unityLogger.logProlog(DELIMITERSEND + message);
        }

        Debug.Log("Queue Count: " + waitingObjects.Count + ", Process Name: " + prolog.ProcessName + ", IsRunning: " + !prolog.HasExited);

        if(waitingObjects.Count > 3) {

            sw.WriteLine(waitingObjects.Peek().Query);
            sw.Flush();
        }
    }

    /// <summary>
    /// Expose the kill signal to other classes.
    /// </summary>
    public void Kill()
    {
        prolog.Kill();
    }
}