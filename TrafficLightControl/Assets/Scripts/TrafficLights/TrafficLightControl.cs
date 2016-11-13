using UnityEngine;
using System.Collections;
using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Timers;

public class TrafficLightControl : MonoBehaviour, IProlog, IIntervalMultiplierUpdate
{

    public enum Crossroads
    {
        a,
        b
    }

    public TrafficLight[] TrafficLights;

    public PrologWrapper PrologInterface;

    private const string GREEN = "G = ";
    private const string AMPEL = "ampel";
    private const string NEXT_PHASE = "getnextPhase(";
    private const string NEUES_EREIGNIS = "neuesEreignis(";

    public Crossroads Crossroad = Crossroads.a;

    private List<string> _greenTrafficLights;
    
    private Timer _phaseTimer;
    private float _multiplier = 1.0f;

    // Use this for initialization
    void Start()
    {
        _greenTrafficLights = new List<string>();

        _phaseTimer = new Timer
        {
            Interval = 2000,
            AutoReset = true
        };
        _phaseTimer.Elapsed += TimerEvent;
        _phaseTimer.Start();
    }

    void Update()
    {
        //print("remaining=" + phaseTimer.Remaining + ", Enabled=" + phaseTimer.Enabled);
        _phaseTimer.Update(Time.deltaTime);
    }

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit()
    {
        if (_phaseTimer != null)
            _phaseTimer.Stop();
    }

    /// <summary>
    /// processing recived data
    /// </summary>
    /// <param name="recivedData"></param>
    public void ReciveDataFromProlog(string recivedData)
    {
        //recivedData is emtry or empty list
        if (string.IsNullOrEmpty(recivedData) || recivedData.Contains("G = [].") || recivedData.Equals("true") ||
            recivedData.Equals("false") || recivedData.Equals("true.") || recivedData.Equals("false."))
            return;


        //\[\[.*\],.*,\d{1,3}\]
        //print("R:" + recivedData);
        try
        {
            var recivedDataWithOutVar = recivedData.Replace(GREEN, "");

            //print("Rwov:" + recivedData);

            var arrayStart = recivedDataWithOutVar.LastIndexOf("[");
            var arrayEnd = recivedDataWithOutVar.IndexOf("]");
            var arrayLength = arrayEnd - arrayStart;

            //print("S:"+ arrayStart + ", E:"+ arrayEnd + ", L:"+ arrayLength);

            var greenTrafficLightsArray = recivedDataWithOutVar.Substring(arrayStart, arrayLength);
            greenTrafficLightsArray = greenTrafficLightsArray.Replace("[", "").Replace("]", "");

            var stringSeparators = new string[] {","};
            var splits = greenTrafficLightsArray.Split(stringSeparators, StringSplitOptions.None);

            //clear old green elements
            _greenTrafficLights.Clear();

            //remove unnecessary symbols and elements and add element to list
            foreach (var s in splits)
            {
                string tmp;

                if (string.IsNullOrEmpty(tmp = Parse(s)))
                    continue;

                _greenTrafficLights.Add(tmp);
            }

            //change states...
            foreach (var l in TrafficLights)
            {
                //entry trafficlight in green trafficlight list?
                var name = l.Name.ToString().ToLower();
                if (_greenTrafficLights.Contains(name)){
                    //print(name + " to green.");
                    l.switchToGreen();
                }

                else {
                    l.switchToRed();
                }
                    
            }

            //phase time from response
            var phaseTimeStartIndex = recivedDataWithOutVar.LastIndexOf(",");

            var nextPhaseTimeString =
                recivedDataWithOutVar.Substring(phaseTimeStartIndex).Replace("].", "").Replace(",", "").Trim();


            var nextPhaseTime = Convert.ToInt32(nextPhaseTimeString);

            _phaseTimer.Interval = (long) (nextPhaseTime*1000*_multiplier);
            // wait because asych
            Thread.Sleep(100);
            _phaseTimer.Start();
        }
        catch (Exception ex)
        {
            _phaseTimer.Interval = 15000*_multiplier;
            Thread.Sleep(100);
            _phaseTimer.Start();

            print("#######################################################################");
            print(ex);
            print("#######################################################################");
        }
    }

    private static string Parse(string s)
    {
        return s.Replace(")", "").Replace(".", "").Replace(",", "").Trim();
    }

    /// <summary>
    /// Call this function to got to the next state
    /// </summary>
    private void NextState()
    {
        var query = NEXT_PHASE + Crossroad + ", G).";
        PrologInterface.QueryProlog(query, this);
    }


    private void TimerEvent(object sender, System.EventArgs e)
    {
        NextState();
    }

    /// <summary>
    /// Call prolog, that a new event was triggered at this crossroad
    /// </summary>
    /// <param name="trigger"></param>
    public void EventWasTriggered(string trigger)
    {
        var query = NEUES_EREIGNIS + Crossroad + ", " + trigger + ").";
        PrologInterface.QueryProlog(query);

        if (!_phaseTimer.Enabled)
            print(">>>>>>>>>>>>>>>>>> TIMER IS NOT RUNNING <<<<<<<<<<<<<<<<<<<<<<<<<");
    }

    public void updateMultiplier(float value)
    {
        _multiplier = value;
        //print("LightControlValue" + value);
        if (_phaseTimer != null)
            _phaseTimer.Remaining *= value;
    }
}