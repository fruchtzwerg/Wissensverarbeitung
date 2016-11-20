using UnityEngine;
using System;

public class TrafficLight : MonoBehaviour, IIntervalMultiplierUpdate
{
    public enum States
    {
        Off,
        Red,
        Yellow,
        Green,
        RedAndOrange
    }

    public enum Lights
    {
        b3,
        fa1,
        fa2,
        fa4,
        fa5,
        fa7,
        fa10,
        fa11,
        fg3,
        fg6,
        fg8,
        fg9,
        k1,
        k2,
        k3,
        k4,
        k5,
        k6,
        k7,
        k8,
        k9,
        k10,
        k11,
        k12,
        k13,
        k14,
        boomgate
    }

    public Lights Name;

    public GameObject RedLight;
    public GameObject OrangeLight;
    public GameObject GreenLight;

    protected Renderer RendRed;
    protected Renderer RendOrange;
    protected Renderer RendGreen;
    
    protected Timer TimerGreen;
    protected Timer TimerRed;

    protected BoxCollider Collider;
    
    protected States OldState = States.Off;

    public long IntervalGreen = 3000;
    public long IntervalRed = 2000;

    public States State { get; protected set; }

    public InductionLoop[] inductionLoops;

    // Use this for initialization
    void Awake()
    {
        // init collider
        Collider = GetComponent<BoxCollider>();

        //init Rendere with different materials
        if (RedLight)
            RendRed = RedLight.GetComponent<Renderer>();

        if (OrangeLight)
            RendOrange = OrangeLight.GetComponent<Renderer>();

        if (GreenLight)
            RendGreen = GreenLight.GetComponent<Renderer>();

        InitTimerGreen();
        InitTimerRed();

    }

    void Start()
    {

        State = States.Red;
        SwitchState();
    }


    protected virtual void InitTimerGreen()
    {
        TimerGreen = new Timer
        {
            Interval = IntervalGreen,
            AutoReset = false
        };
        TimerGreen.Elapsed += TimerEventToGreen;
    }

    protected virtual void InitTimerRed()
    {
        TimerRed = new Timer
        {
            Interval = IntervalRed,
            AutoReset = false
        };
        TimerRed.Elapsed += TimerEventToRed;
    }



    // Update is called once per frame
    void Update()
    {
        //print("green:delta=" + Time.deltaTime + ", remaining=" + timerGreen.Remaining + ", doCount=" + timerGreen._doCount);
        //print("red:delta=" + Time.deltaTime + ", remaining=" + timerRed.Remaining + ", doCount=" + timerRed._doCount);
        TimerGreen.Update(Time.deltaTime);
        TimerRed.Update(Time.deltaTime);
        SwitchState();
    }


    /// <summary>
    /// switch state from red to green
    /// </summary>
    public virtual void SwitchToGreen()
    {
        //only if red or in some sec red
        if (State != States.RedAndOrange && State != States.Green)
        {
            State = States.RedAndOrange;
            TimerGreen.Start();
        }
    }


    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public virtual void SwitchToRed()
    {
        //only if red or in some sec red
        if (State != States.Yellow && State != States.Red)
        {
            State = States.Yellow;
            TimerRed.Start();
        }
    }


    protected virtual void TimerEventToGreen(object source, EventArgs e)
    {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        State = States.Green;
        TimerGreen.Stop();
    }

    protected virtual void TimerEventToRed(object source, EventArgs e)
    {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        State = States.Red;
        TimerRed.Stop();
    }


    /// <summary>
    /// switch state and emission
    /// </summary>
    protected virtual void SwitchState()
    {

        if (OldState != State)
        {
            OldState = State;
            //switch emissioncolor of gameobjects
            switch (State)
            {
                case States.Red:
                    RendRed.material.SetColor("_EmissionColor", Color.white);
                    RendOrange.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Yellow:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendOrange.material.SetColor("_EmissionColor", Color.white);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    EnableCollider();
                    break;
                case States.Green:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendOrange.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.white);
                    EnableCollider(false);
                    break;
                case States.RedAndOrange:
                    RendRed.material.SetColor("_EmissionColor", Color.white);
                    RendOrange.material.SetColor("_EmissionColor", Color.white);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                default:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendOrange.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
            }

        }
    }


    /// <summary>
    /// Move the collider out of the way.
    /// </summary>
    /// <param name="enable"></param>
    protected void EnableCollider(bool enable = true)
    {
        var offset = new Vector3(0, 100, 0);

        if (enable)
            Collider.center += offset;
        else
            Collider.center -= offset;

        foreach(var tmp in inductionLoops) {
            tmp.enableEventCollider(enable);
        }
    }

    public void updateMultiplier(float value)
    {
        if(TimerGreen == null || TimerRed == null)
            return;

        TimerGreen.Interval = (long) (IntervalGreen*value);
        TimerRed.Interval = (long) (IntervalRed*value);
    }
}