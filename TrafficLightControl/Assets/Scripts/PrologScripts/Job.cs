using System.Diagnostics;
using System.IO;
using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Debug = UnityEngine.Debug;

public class Job
{

    private Process _prolog;
    private StreamWriter _sw;
    private const string EXE = "swipl.exe";
    public const string DELIMITER_SEND = "?- ";
    public const string DELIMITER_RECIVE = "    ";

    private Queue<WaitingObject> _queue;

    private UnityLogger _unityLogger;

    public Job(UnityLogger logger)
    {
        _unityLogger = logger;
        _queue = new Queue<WaitingObject>();

        _prolog = new Process
        {
            StartInfo =
            {
                FileName = EXE,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardInput = true,
                CreateNoWindow = true,
                Arguments = "-q"
            }
        };

        // find the exe path
        if (!FindSwiExe()) return;


        // start the process
        _prolog.Start();

        // IO: register STDIN of the process as a streamwriter
        _sw = new StreamWriter(_prolog.StandardInput.BaseStream, Encoding.ASCII)
        {
            AutoFlush = true
        };

        // make sure Encodings are compatible
        Console.OutputEncoding = Encoding.ASCII;
        Console.InputEncoding = Encoding.ASCII;

        // register the recieved listener for STDOUT of the process
        _prolog.OutputDataReceived += (sender, args) => {
            PrintToUnity(args.Data);
        };

        // start listening on STDOUT
        _prolog.BeginOutputReadLine();
    }

    /// <summary>
    /// Searches for the swipl.exe path by different means.
    /// 
    /// exe search pattern:
    /// current dir > path env-var > default install dir > not found
    /// </summary>
    /// <returns>true if found, else false</returns>
    private bool FindSwiExe()
    {
        // if file not exists in current folder
        if (!File.Exists(_prolog.StartInfo.FileName))
        {
            // get exe from path
            var path = Environment.GetEnvironmentVariable("PATH");

            _prolog.StartInfo.FileName = path.Split(';')
                .Select(split => Path.Combine(split, EXE))
                .FirstOrDefault(File.Exists);
        } 
        
        // if not found in path get exe at default install dirs
        if (string.IsNullOrEmpty(_prolog.StartInfo.FileName))
        {
            if (File.Exists(@"C:\Program Files\swipl\bin\swipl.exe"))
                _prolog.StartInfo.FileName = @"C:\Program Files\swipl\bin\swipl.exe";
            else if (File.Exists(@"C:\Program Files (x86)\swipl\bin\swipl.exe"))
                _prolog.StartInfo.FileName = @"C:\Program Files (x86)\swipl\bin\swipl.exe"; 
        }

        // swipl.exe not found
        if (string.IsNullOrEmpty(_prolog.StartInfo.FileName)) 
        {
            Debug.Log("SWI-Prolog executable not found!");
            return false;
        }
        return true;
    }


    /// <summary>
    /// consult prolog knowledgebase file
    /// </summary>
    /// <param name="path"></param>
    public void ConsultFile(string path)
    {
        // exit if process not running
        if (_prolog == null)
        {
            Debug.Log("Prolog-Process not running! Exiting now.");
            return;
        }
        
        // exit if .pl file does not exist
        if (!File.Exists(path))
        {
            Debug.Log(string.Format("File at {0} does not exist! Exiting now.", path));
            return;
        }

        // consult the file
        string query = "consult('" + path + "').";
        _sw.WriteLine(query);
        _sw.Flush();

        // log prolog input
        _unityLogger.LogProlog(DELIMITER_SEND + query);
    }

    /// <summary>
    /// Print a string to Unity's console and logfile.
    /// </summary>
    /// <param name="message"></param>
    private void PrintToUnity(string message)
    {
        // only print sensible messages
        if (string.IsNullOrEmpty(message.Trim()))
            return;

        // print to console and use delimiter
        //print(DELIMITER_RECIVE + message);

        _unityLogger.LogProlog(DELIMITER_RECIVE + message);


        if (_queue.Count > 0)
        {
            ReplySender(message);
        }

    }

    /// <summary>
    /// send reply to sender
    /// </summary>
    /// <param name="message">message to sender as reply</param>
    private void ReplySender(string message)
    {
        var waitingObject = _queue.Dequeue();

        //if sender is waiting for response
        if (waitingObject != null && waitingObject.Sender != null)
        {
            //send reply
            waitingObject.Sender.ReciveDataFromProlog(message);
        }

        //if there a still other querys to prolog...
        if (_queue.Count > 0)
        {
            //get fist and query prolog
            var next = _queue.Peek();

            _sw.WriteLine(next.Query);
            _sw.Flush();

            _unityLogger.LogProlog(DELIMITER_SEND + next.Query);

            Debug.Log("Process Name: " + _prolog.ProcessName + ", Exit: " + _prolog.HasExited);
        }

        //print("Queue Count 2: " + waitingObjects.Count);
    }


    /// <summary>
    /// Query swi-prolog for something.
    /// </summary>
    /// <param name="message">The query string.</param>
    /// <param name="sender">The sender object</param>
    public void Query(string message, IProlog sender = null)
    {
        // std-in of prolog still registered?
        // OR: message null or empty?
        if (_sw == null || string.IsNullOrEmpty(message.Trim()))
            return;

        // make sure the query string ends with a '.'
        if (!message.Trim().EndsWith("."))
            message = message + ".";

        //set sender for reply
        _queue.Enqueue(new WaitingObject(message, sender));
        //this.sender = sender;

        //query prolog, if the is no other query
        if (_queue.Count == 1)
        {
            // write the query to prolog console and execute
            _sw.WriteLine(message);
            _sw.Flush();

            _unityLogger.LogProlog(DELIMITER_SEND + message);
        }

        Debug.Log("Queue Count: " + _queue.Count + ", Process Name: " + _prolog.ProcessName + ", IsRunning: " +
                  !_prolog.HasExited);

        if (_queue.Count > 3)
        {
            _sw.WriteLine(_queue.Peek().Query);
            _sw.Flush();
        }
    }

    /// <summary>
    /// Expose the kill signal to other classes.
    /// </summary>
    public void Kill()
    {
        if(_prolog != null && !_prolog.HasExited)
            _prolog.Kill();
    }
}