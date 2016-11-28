using UnityEngine;

public class EventTrigger : MonoBehaviour, IIntervalMultiplierUpdate
{

    public long TimerInterval = 10000;
    public TrafficLightControl TrafficLightControl;

    private Timer timer;

    public Events[] events;
    public float[] probabilities;

    private System.Random rand;


    public enum Events {
        boomgate,
        b1,
        b3,
        fa1,
        fa4,
        fa10,
        k4,
        k6,
        k8,
        k10,
        k12,
        noAction

    }

    // Use this for initialization
    void Start()
    {
        // if TrafficLightControl is same object, init it
        if (!TrafficLightControl)
            TrafficLightControl = GetComponent<TrafficLightControl>();

        timer = new Timer
        {
            Interval = TimerInterval,
            AutoReset = true
        };
        timer.Elapsed += TimerElapsed;
        timer.Start();
        //print("remaining=" + timer.Remaining);

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

    private void TimerElapsed(object source, System.EventArgs e)
    {
        for (var i = 0; i < events.Length; i++)
        {

            var value = (float)rand.NextDouble();

            if (value <= probabilities[i] && TrafficLightControl != null)
            {
                TrafficLightControl.EventWasTriggered(events[i]);
            }
        }
    }

    public void updateMultiplier(float value)
    {
        if(timer != null)
            timer.Interval = (long)(TimerInterval * value);
    }
}
