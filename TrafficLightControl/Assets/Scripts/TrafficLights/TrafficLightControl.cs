using UnityEngine;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

public class TrafficLightControl : MonoBehaviour, IProlog, IIntervalMultiplierUpdate
{

    public enum Crossroads
    {
        a,
        b
    }

    public TrafficLight[] TrafficLights;

    public PrologWrapper PrologInterface;

    //private const string GREEN = "G = ";
    //private const string LIGHT_PREFIX = "ampel";
    //private const string NEXT_PHASE_PREFIX = "getnextPhase(";
    //private const string NEW_EVENT_PREFIX = "neuesEreignis(";

    public Crossroads Crossroad = Crossroads.a;

    private List<string> _greenTrafficLights;

    private Timer _phaseTimer;
    private float _multiplier = 1.0f;
    private string _receivedDataWithOutVar;

    public int StartInterval = 2000;
    private PhaseInfo.JunctionPhase _currentPhase;

    // Use this for initialization
    void Start()
    {
        _greenTrafficLights = new List<string>();

        _phaseTimer = new Timer
        {
            Interval = StartInterval,
            AutoReset = true
        };
        _phaseTimer.Elapsed += NextState;
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
    /// processing received data
    /// </summary>
    /// <param name="data"></param>
    public void ReceiveDataFromProlog(string data)
    {
        //receivedData is empty or empty list
        if (IsValidData(data))
            return;

        try
        {
            // Parse data from prolog and set new state
            var state = PrologWrapper.ParsePhaseInfo(data);
            _currentPhase = state.Phase;

            //change states
            ChangeStates(state.GreenLightes);

            // reset the timer for the next phase
            SetTimer(state.Duration);
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


    /// <summary>
    /// Checks if receivedData is a valid response from prolog.
    /// </summary>
    /// <param name="receivedData"></param>
    /// <returns>true: valid | false: invalid</returns>
    private static bool IsValidData(string receivedData)
    {
        return string.IsNullOrEmpty(receivedData) || receivedData.Contains("G = [].") || receivedData.Equals("true") ||
               receivedData.Equals("false") || receivedData.Equals("true.") || receivedData.Equals("false.");
    }


    /// <summary>
    /// Converts a single prolog response string into a
    /// string array.
    /// </summary>
    /// <returns>prolog response as string[]</returns>
    private string[] ArrayFromData()
    {
        var arrayStart = _receivedDataWithOutVar.LastIndexOf("[");
        var arrayEnd = _receivedDataWithOutVar.IndexOf("]");
        var arrayLength = arrayEnd - arrayStart;

        // if start, end or length is negative -> return
        if (arrayStart < 0 || arrayEnd < 0 || arrayLength < 0)
            return null;

        //print("S:"+ arrayStart + ", E:"+ arrayEnd + ", L:"+ arrayLength);

        var greenTrafficLightsArray = _receivedDataWithOutVar.Substring(arrayStart, arrayLength);
        greenTrafficLightsArray = greenTrafficLightsArray.Replace("[", "").Replace("]", "");

        var stringSeparators = new[] {","};
        return greenTrafficLightsArray.Split(stringSeparators, StringSplitOptions.None);
    }


    /// <summary>
    /// Sets the phase-timer accoarding to prolog response.
    /// </summary>
    private void SetTimer(int duration)
    {
        // reset timer
        _phaseTimer.Interval = (long) (duration*_multiplier);
        // wait because asych
        Thread.Sleep(100);
        _phaseTimer.Start();
    }


    /// <summary>
    /// Clears the list of green lights and rebuilds it with new
    /// lights from a string[].
    /// </summary>
    /// <param name="splits">string[] of lights</param>
    private void RebuildLightsList(IEnumerable<string> splits)
    {
        //clear old green elements
        _greenTrafficLights.Clear();

        //remove unnecessary symbols and elements and add element to list
        foreach (var s in splits)
        {
            string tmp;

            if (string.IsNullOrEmpty(tmp = Trim(s)))
                continue;

            _greenTrafficLights.Add(tmp);
        }
    }


    /// <summary>
    /// Advances states of all lights in TrafficLights
    /// </summary>
    private void ChangeStates(TrafficLight.Lights[] greenLights)
    {
        foreach (var l in TrafficLights)
        {
            //entry trafficlight in green trafficlight list?
            //var name = l.Name.ToString().ToLower();

            if (greenLights.Any(x => x == l.Name))
                l.SwitchToGreen();
            else
                l.SwitchToRed();
        }
    }


    /// <summary>
    /// Replaces useless delimiters with empty strings.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    private static string Trim(string s)
    {
        return s.Replace(")", "").Replace(".", "").Replace(",", "").Trim();
    }

    /// <summary>
    /// Call this function to got to the next state
    /// </summary>
    private void NextState(object sender = null, EventArgs e = null)
    {
        var query = PrologWrapper.BuildQuery(PrologWrapper.QueryType.NextPhase, Crossroad, _currentPhase, "G");
        PrologInterface.QueryProlog(query, this);
    }


    /// <summary>
    /// Call prolog, that a new event was triggered at this crossroad
    /// </summary>
    /// <param name="trigger"></param>
    public bool EventWasTriggered(EventTrigger.Events trigger)
    {
        var query = PrologWrapper.BuildQuery(PrologWrapper.QueryType.Event, Crossroad, _currentPhase, trigger.ToString());
        PrologInterface.QueryProlog(query);

        return true;
    }


    public void updateMultiplier(float value)
    {
        _multiplier = value;
        //print("LightControlValue" + value);

        if (_phaseTimer != null)
            _phaseTimer.Remaining *= value;
    }
}