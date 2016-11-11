using UnityEngine;
using System;

public class TrafficLight : MonoBehaviour, IIntervalMultiplierUpdate
{



    public GameObject RedLight;
    public GameObject OrangeLight;
    public GameObject GreenLight;


    public Shader shader;

    protected Renderer rendRed;
    protected Renderer rendOrange;
    protected Renderer rendGreen;

    protected Color black = new Color(0, 0, 0);
    protected Color red = new Color(0.7f, 0, 0);
    protected Color orange = new Color(1f, 1f, 0);
    protected Color green = new Color(0, 1f, 0);

    protected Timer timerGreen;
    protected Timer timerRed;

    private BoxCollider _collider;
    private Material matGreen;
    private Material matYellow;
    private Material matRed;

    protected States state = States.Off;
    protected States oldState = States.Off;

    public long IntervalGreen = 3000;
    public long IntervalRed = 2000;

    public States State
    {
        get { return state; }
    }

    public enum States
    {
        Red,
        Orange,
        Green,
        RedAndOrange,
        Off,
        On
    }

    // Use this for initialization
    void Start()
    {
        // init collider
        _collider = GetComponent<BoxCollider>();

        // init materials
        matGreen = Resources.Load("TrafficLightGreen", typeof(Material)) as Material;
        matYellow = Resources.Load("TrafficLightYellow", typeof(Material)) as Material;
        matRed = Resources.Load("TrafficLightRed", typeof(Material)) as Material;

        //init Rendere with different materials
        if (RedLight)
        {
            rendRed = RedLight.GetComponent<Renderer>();
            rendRed.material = matRed;
        }

        if (OrangeLight)
        {
            rendOrange = OrangeLight.GetComponent<Renderer>();
            rendOrange.material = matYellow;
        }

        if (GreenLight)
        {
            rendGreen = GreenLight.GetComponent<Renderer>();
            rendGreen.material = matGreen;
        }

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
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.Orange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    EnableCollider();
                    break;
                case States.Green:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    EnableCollider(false);
                    break;
                case States.RedAndOrange:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.On:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
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
        timerGreen.Interval = (long) (IntervalGreen*value);
        timerRed.Interval = (long) (IntervalRed*value);
    }
}