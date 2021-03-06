﻿using UnityEngine;
using System;
using System.Linq;
using System.Threading;

public class TrafficLightControl : MonoBehaviour, IProlog, IIntervalMultiplierUpdate
{


    public enum Crossroads
    {
        // lowercase needed here
        a,
        b
    }

    public TrafficLight[] TrafficLights;

    public PrologWrapper PrologInterface;

    public Crossroads Crossroad = Crossroads.a;

    private Timer _phaseTimer;
    private float _multiplier = 1.0f;

    public int StartInterval = 2000;
    private SequenceInfo.JunctionSequence _currentSequence;
    private int duration;
    private int _count;

    // Use this for initialization
    void Start()
    {
        _phaseTimer = new Timer
        {
            Interval = StartInterval,
            AutoReset = true
        };
        _phaseTimer.Elapsed += NextSequence;
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
        PrologInterface.Log(UnityLogger.DELIMITER_RECEIVE + data, Crossroad.ToString());

        //receivedData is empty or empty list
        if (IsValidData(data))
            return;

        try
        {
            // Parse data from prolog and set new state
            var state = PrologWrapper.ParseSequenceInfo(data);
            _currentSequence = state.Sequence;

            //change states
            ChangeStates(state.GreenLightes);

            // reset the timer for the next phase
            duration = state.Duration;
        }
        catch (Exception ex)
        {
            duration = 15000;

            print("#######################################################################");
            print(ex);
            print("#######################################################################");
        }
        finally
        {
            //print(duration);
            SetTimer(duration);
        }
    }


    /// <summary>
    /// Checks if receivedData is a valid response from prolog.
    /// </summary>
    /// <param name="receivedData"></param>
    /// <returns>true: valid | false: invalid</returns>
    private static bool IsValidData(string receivedData)
    {
        var data = receivedData.Trim();
        return string.IsNullOrEmpty(data) || data.Contains("G = [].") || data.Equals("true") ||
               data.Equals("false") || data.Equals("true.") || data.Equals("false.");
    }


    /// <summary>
    /// Sets the phase-timer accoarding to prolog response.
    /// </summary>
    private void SetTimer(int duration)
    {
        // reset timer
        _phaseTimer.Interval = (long) (duration*_multiplier);
        // wait because asynch
        Thread.Sleep(1000);
        _phaseTimer.Start();
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
    /// Call this function to got to the next state
    /// </summary>
    private void NextSequence(object sender = null, EventArgs e = null)
    {
        var result = "G" + Crossroad + _count++;
        var query = PrologWrapper.BuildQuery(PrologWrapper.QueryType.NextSequence, Crossroad, _currentSequence, result);
        PrologInterface.QueryProlog(query, this, Crossroad.ToString());
    }


    /// <summary>
    /// Call prolog, that a new event was triggered at this crossroad
    /// </summary>
    /// <param name="trigger"></param>
    public bool EventWasTriggered(EventTrigger.Events trigger)
    {
        var query = PrologWrapper.BuildQuery(PrologWrapper.QueryType.Event, Crossroad, _currentSequence, trigger.ToString());
        PrologInterface.QueryProlog(query, this, Crossroad.ToString());

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