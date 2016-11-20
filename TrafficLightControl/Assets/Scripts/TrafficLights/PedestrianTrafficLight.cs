using UnityEngine;
using System;

public class PedestrianTrafficLight : TrafficLight, IIntervalMultiplierUpdate {

    public PedestrianButton[] PedestrianTrafficLightButtons;

    public long Interval = 2000;
    public float OffsetAudioSource = 25;

    private GameObject audioSourceHolder;
    private AudioSource audioSource;
    private AudioClip clip1;
    private AudioClip clip2;

    protected override void InitTimerGreen() {
        TimerGreen = new Timer
        {
            Interval = Interval,
            AutoReset = false
        };
        TimerGreen.Elapsed += TimerEventToGreen;

        initAudio();
    }

    /// <summary>
    /// Initialise the audiosource
    /// </summary>
    private void initAudio() {

        //Gameobject to hold the audiosource
        //this gameobject is + Offset over the pedestrian gameobject
        audioSourceHolder = GameObject.CreatePrimitive(PrimitiveType.Sphere);
        audioSourceHolder.GetComponent<MeshRenderer>().enabled = false;
        audioSourceHolder.name = this.name + "_AudioSource";
        audioSourceHolder.transform.position = new Vector3(transform.position.x, transform.position.y + OffsetAudioSource, transform.position.z);

        audioSourceHolder.transform.SetParent(gameObject.transform);

        audioSourceHolder.AddComponent<AudioSource>();

        try {           
            clip1 = Resources.Load<AudioClip>("Sound/pedestrian-crossing_1");
            clip2 = Resources.Load<AudioClip>("Sound/pedestrian-crossing_2");
        }
        catch(Exception e) {
            print("ERROR: " + e);
        }

        if (clip1 != null)
            clip1.LoadAudioData();
        else
            print("clip1 is null");
        if(clip2 != null)
            clip2.LoadAudioData();
        else
            print("clip2 is null");

        audioSource = audioSourceHolder.GetComponent<AudioSource>();
        audioSource.playOnAwake = true;
        audioSource.loop = true;
        audioSource.maxDistance = 8.0f;
        audioSource.minDistance = 0.1f;

        AnimationCurve curve = new AnimationCurve();
        curve.AddKey(0, 1);
        curve.AddKey(7, 0.1f);
        curve.AddKey(8, 0);

        audioSource.SetCustomCurve(AudioSourceCurveType.CustomRolloff, curve);
        audioSource.rolloffMode = AudioRolloffMode.Custom;
        audioSource.clip = clip1;
        audioSource.spatialBlend = 1f;
        audioSource.Play();
    }

    /// <summary>
    /// switch state from red to green
    /// </summary>
    public override void SwitchToGreen() {
        //only if red or in some sec red
        if (State != States.Green) {
            TimerGreen.Start();
        }
    }

    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public override void SwitchToRed() {
        //only if red or in some sec red
        if (State != States.Red) {
            State = States.Red;
        }
    }

    /// <summary>
    /// switch state and emission
    /// </summary>
    protected override void SwitchState() {

        if (OldState != State) {
            OldState = State;
            //switch emissioncolor of gameobjects
            switch (State) {
                case States.Red:
                    RendRed.material.SetColor("_EmissionColor", Color.white);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);

                    changeAudioClip(true);
                    break;
                case States.Yellow:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Green:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.white);

                    switchOffPedestrianTrafficLightLights();

                    changeAudioClip(false);
                    break;
                case States.RedAndOrange:
                    RendRed.material.SetColor("_EmissionColor", Color.white);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                default:
                    RendRed.material.SetColor("_EmissionColor", Color.black);
                    RendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
            }
        }
    }

    private void changeAudioClip(bool clip) {
        if (clip)
            audioSource.clip = clip1;
        else
            audioSource.clip = clip2;
        audioSource.Play();
    }

    /// <summary>
    /// switch off all PedestrianTrafficLight Lights
    /// </summary>
    private void switchOffPedestrianTrafficLightLights() {
        foreach(var tmp in PedestrianTrafficLightButtons) {
            tmp.switchOffEmission();
        }
    }

    public new void updateMultiplier(float value)
    {
        TimerGreen.Interval = (long)(Interval * value);
        audioSource.pitch = 1f / value;
    }
}
