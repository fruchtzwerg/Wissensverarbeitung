using System.Timers;
using UnityEngine;

public class EventTrigger : MonoBehaviour {

    public long TimerInterval = 10000;
    public GameObject TrafficLightControl;

    private Timer timer;

    public string[] events;
    public float[] probabilities;

    private System.Random rand;

    private TrafficLightControl control;

    // Use this for initialization
    void Start () {
        timer = new Timer();        
        timer.Interval = TimerInterval;
        timer.Elapsed += timerElapsed;
        timer.AutoReset = true;
        timer.Start();

        rand = new System.Random();

        control = TrafficLightControl.GetComponent<TrafficLightControl>();

        if (events.Length != probabilities.Length)
            print("not all events have propabilities!");
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit() {
        timer.Stop();
    }

    private void  timerElapsed(object source, System.EventArgs e) {

        for (int i = 0; i< events.Length; i++) {

            float value = (float) rand.NextDouble();

            if (value <= probabilities[i]) {
                print("Event " + events[i] + " was triggert.");

                control.EventWasTriggered(events[i]);
            }                
        }
    }
}
