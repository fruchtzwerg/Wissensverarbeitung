using UnityEngine;
using System;

public class TrafficLight : MonoBehaviour, IIntervalMultiplierUpdate
{
    public enum States
    {
        Red,
        Orange,
        Green,
        RedAndOrange,
        Off,
        On,
        Open,
        Opening,
        Closed,
        Closing
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

    protected Renderer rendRed;
    protected Renderer rendOrange;
    protected Renderer rendGreen;
    
    protected Timer timerGreen;
    protected Timer timerRed;

    private BoxCollider _collider;

    protected States state = States.Off;
    protected States oldState = States.Off;

    public long IntervalGreen = 3000;
    public long IntervalRed = 2000;

    public States State
    {
        get { return state; }
    }

    // Use this for initialization
    void Start()
    {
        // init collider
        _collider = GetComponent<BoxCollider>();

        //init Rendere with different materials
        if (RedLight)
            rendRed = RedLight.GetComponent<Renderer>();

        if (OrangeLight)
            rendOrange = OrangeLight.GetComponent<Renderer>();

        if (GreenLight)
            rendGreen = GreenLight.GetComponent<Renderer>();

        InitTimerGreen();
        InitTimerRed();

        state = States.Red;
        switchState();
    }


    protected virtual void InitTimerGreen()
    {
        timerGreen = new Timer
        {
            Interval = IntervalGreen,
            AutoReset = false
        };
        timerGreen.Elapsed += timerEventToGreen;
    }

    protected virtual void InitTimerRed()
    {
        timerRed = new Timer
        {
            Interval = IntervalRed,
            AutoReset = false
        };
        timerRed.Elapsed += timerEventToRed;
    }



    // Update is called once per frame
    void Update()
    {
        //print("green:delta=" + Time.deltaTime + ", remaining=" + timerGreen.Remaining + ", doCount=" + timerGreen._doCount);
        //print("red:delta=" + Time.deltaTime + ", remaining=" + timerRed.Remaining + ", doCount=" + timerRed._doCount);
        timerGreen.Update(Time.deltaTime);
        timerRed.Update(Time.deltaTime);
        switchState();
    }


    /// <summary>
    /// switch state from red to green
    /// </summary>
    public virtual void switchToGreen()
    {
        //only if red or in some sec red
        if (State != States.RedAndOrange && State != States.Green)
        {
            state = States.RedAndOrange;
            //switchState();
            timerGreen.Start();
        }
    }


    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public virtual void switchToRed()
    {
        //only if red or in some sec red
        if (State != States.Orange && State != States.Red)
        {
            state = States.Orange;
            //switchState();
            timerRed.Start();
        }
    }


    protected virtual void timerEventToGreen(object source, EventArgs e)
    {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        state = States.Green;
        timerGreen.Stop();
    }

    protected virtual void timerEventToRed(object source, EventArgs e)
    {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        state = States.Red;
        timerRed.Stop();
    }


    /// <summary>
    /// switch state and emission
    /// </summary>
    protected virtual void switchState()
    {

        if (oldState != State)
        {
            oldState = State;
            //switch emissioncolor of gameobjects
            switch (State)
            {
                case States.Red:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendOrange.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Orange:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendOrange.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    EnableCollider();
                    break;
                case States.Green:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendOrange.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);
                    EnableCollider(false);
                    break;
                case States.RedAndOrange:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendOrange.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.On:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendOrange.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendOrange.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
            }

        }
    }


    /// <summary>
    /// Move the collider out of the way.
    /// </summary>
    /// <param name="enable"></param>
    private void EnableCollider(bool enable = true)
    {
        var offset = new Vector3(0, 100, 0);

        if (enable)
            _collider.center += offset;
        else
            _collider.center -= offset;
    }

    public void updateMultiplier(float value)
    {
        if(timerGreen == null || timerRed == null)
            return;

        timerGreen.Interval = (long) (IntervalGreen*value);
        timerRed.Interval = (long) (IntervalRed*value);
    }
}