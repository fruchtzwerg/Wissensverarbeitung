using UnityEngine;
using System.Diagnostics;
using System.IO;
using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

public class Job : MonoBehaviour
{

    private Process prolog;
    private StreamWriter sw;
    public const string DELIMITERSEND = "--> ";
    public const string DELIMITERRECIVE = "<-- ";

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
        prolog.StartInfo.FileName = @"C:\Program Files\swipl\bin\swipl.exe";

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

        WriteLogFile(DELIMITERSEND + query);       
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
        print(DELIMITERRECIVE + message);

        WriteLogFile(DELIMITERRECIVE + message);


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

        if(waitingObject != null && waitingObject.Sender != null) {
            //send reply
            waitingObject.Sender.ReciveDataFromProlog(message);

            //if there a still other querys to prolog...
            if (waitingObjects.Count > 0) {
                //get fist and query prolog
                var next = waitingObjects.Peek();
                this.Query(next.Query);
            }
        }
        else {
            print("Waiting Object or sender is null!");
        }

        //print("Queue Count 2: " + waitingObjects.Count);
    }

    /// <summary>
    /// Query swi-prolog for something.
    /// </summary>
    /// <param name="message">The query string.</param>
    public void Query(string message)
    {       
        // std-in of prolog still registered?
        // OR: message not null or empty?
        if (sw == null || string.IsNullOrEmpty(message.Trim()))
            return;

        // make sure the query string ends with a '.'
        if (!message.Trim().EndsWith("."))
            message = message + ".";

        // write the query to prolog console and execute
        sw.WriteLine(message);
        sw.Flush();

        WriteLogFile(DELIMITERSEND + message);
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

            WriteLogFile(DELIMITERSEND + message); 
        }

        print("Queue Count: " + waitingObjects.Count);        
    }



    /// <summary>
    /// Write to logfile
    /// </summary>
    /// <param name="message"></param>
    private void WriteLogFile(string message) {        
        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\prolog.log", true)) {
            sw2.WriteLine(DateTime.Now + ": "+ message);
            sw2.Flush();
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