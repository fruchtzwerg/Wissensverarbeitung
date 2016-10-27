﻿using UnityEngine;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System;
using System.Text;

public class Job : MonoBehaviour
{

    private Process prolog;
    private StreamWriter sw;
    public const string DELIMITER = "-->";

    

    /// <summary>
    /// Init the swi-prolog.exe in a console window
    /// </summary>
    public void initPrologProcess()
    {
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



    public void ConsultFile(string path)
    {
        if (prolog == null)
            return;

        print("Try to consult: " + path);
        //Console.WriteLine("Try to consult: " + path);
        printToUnity("Try to consult: " + path);


        sw.WriteLine("consult('" + path + "').");
        sw.Flush();

        //string output = prolog.StandardOutput.ReadToEnd(); 
        //printToUnity("consult '" + Path.GetFileName(path) + "': " + output);
    }

    /// <summary>
    /// Print a string to Unity's console and logfile.
    /// </summary>
    /// <param name="message"></param>
    public void printToUnity(string message)
    {
        // only print sensible messages
        if(string.IsNullOrEmpty(message.Trim()))
            return;

        // print to console and use delimiter
        print(DELIMITER + message);

        // print to logfile
        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\out.txt"))
        {
            sw2.WriteLine(message);
            sw2.Flush();
        }

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
    }


    /// <summary>
    /// Expose the kill signal to other classes.
    /// </summary>
    public void Kill()
    {
        prolog.Kill();
    }
}