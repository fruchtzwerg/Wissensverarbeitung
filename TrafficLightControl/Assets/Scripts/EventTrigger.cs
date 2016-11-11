using System;
using System.Timers;
using UnityEngine;

public class EventTrigger : MonoBehaviour, IIntervalMultiplierUpdate
{

    public long TimerInterval = 10000;
    public TrafficLightControl TrafficLightControl;

    private Timer timer;

    public string[] events;
    public float[] probabilities;

    private System.Random rand;

    private float multiplier = 1f;

    // Use this for initialization
    void Start()
    {
        timer = new Timer();
        timer.Interval = TimerInterval;
        timer.Elapsed += timerElapsed;
        timer.AutoReset = true;
        timer.Start();
        print("remaining=" + timer.Remaining);

        rand = new System.Random();

        if (events.Length != probabilities.Length)
            print("not all events have propabilities!");
    }

    void Update()
    {
        timer.Update(Time.deltaTime);
    }

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit()
    {
        if (timer != null)
            timer.Stop();
    }

    private void timerElapsed(object source, System.EventArgs e)
    {
        for (int i = 0; i < events.Length; i++)
        {

            float value = (float)rand.NextDouble();

            if (value <= probabilities[i] && TrafficLightControl != null)
            {
                TrafficLightControl.EventWasTriggered(events[i]);
            }
        }
    }

    public void updateMultiplier(float value)
    {
        //print("neues Intervall: " + (long)(TimerInterval * value));
        if(timer != null)
            timer.Interval = (long)(TimerInterval * value);
    }
}
