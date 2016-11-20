using UnityEngine;
using System;

public class BoomGateController : BoomGate, IIntervalMultiplierUpdate {

    public BoomGate[] Boomgates;

    public Vector3 AudioSourcePostion = new Vector3(26.232f, 31.86f, 31.58f);

    public TrainSpawner TrainSpawner;

    private GameObject _audioSourceHolder;
    private AudioSource _audioSource;
    private AudioClip _clip1;

    // Use this for initialization
    void Start() {
        State = States.Closed;
        InitAudio();
    }

    // Update is called once per frame
    void Update()
    {
        if (!Boomgates[0]) return;

        if(State != Boomgates[0].State) {
            State = Boomgates[0].State;

            //Play sound
            if (State != States.Open)
                _audioSource.Play();
            else
                _audioSource.Stop();

            if (State == States.Closed)
                TrainSpawner.SpawnTrain();
        }
    }


    public override void SwitchToGreen()
    {
        foreach (var gate in Boomgates)
        {
            if(gate.State == States.Closed)
                gate.State = States.Opening;
        }
    }

    public override void SwitchToRed()
    {
        foreach (var gate in Boomgates)
        {
            if(gate.State == States.Open)
                gate.State = States.Closing;
        }
    }

    /// <summary>
    /// Initialise the audiosource
    /// </summary>
    private void InitAudio() {

        //Gameobject to hold the audiosource
        //this gameobject is + Offset over the pedestrian gameobject
        _audioSourceHolder = GameObject.CreatePrimitive(PrimitiveType.Sphere);
        _audioSourceHolder.GetComponent<MeshRenderer>().enabled = false;
        _audioSourceHolder.name = this.name + "_AudioSource";
        _audioSourceHolder.transform.position = AudioSourcePostion;

        _audioSourceHolder.transform.SetParent(gameObject.transform);

        _audioSourceHolder.AddComponent<AudioSource>();

        try {
            _clip1 = Resources.Load<AudioClip>("/Sound/train_crossing");
        }
        catch (Exception e) {
            print("ERROR: " + e);
        }

        if (_clip1 != null)
            _clip1.LoadAudioData();
        else
            print("clip1 is null");       

        _audioSource = _audioSourceHolder.GetComponent<AudioSource>();
        _audioSource.playOnAwake = true;
        _audioSource.loop = true;
        _audioSource.maxDistance = 10.0f;
        _audioSource.minDistance = 0.3f;

        AnimationCurve curve = new AnimationCurve();
        curve.AddKey(0, 1);
        curve.AddKey(8, 0.1f);
        curve.AddKey(10, 0);

        _audioSource.SetCustomCurve(AudioSourceCurveType.CustomRolloff, curve);
        _audioSource.rolloffMode = AudioRolloffMode.Custom;
        _audioSource.clip = _clip1;
        _audioSource.spatialBlend = 1f;
        _audioSource.Play();        
    }

    public new void updateMultiplier(float value) {
        _audioSource.pitch = 1f/value;
    }
}
