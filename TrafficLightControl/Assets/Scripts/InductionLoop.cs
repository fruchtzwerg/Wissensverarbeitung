using UnityEngine;

public class InductionLoop : MonoBehaviour {

    private BoxCollider _collider;

    public TrafficLightControl TrafficLightControl;
    public EventTrigger.Events TriggerEvent;

    private Vector3 _offset = new Vector3(0, -100, 0);
    private bool _wasTriggeredOnce = false;

	// Use this for initialization
	void Start () {
        _collider = GetComponent<BoxCollider>();        
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    void OnTriggerEnter(Collider other)
    {
        if (_wasTriggeredOnce)
            return;

        _wasTriggeredOnce = TrafficLightControl.EventWasTriggered(TriggerEvent);

    }

    public void EnableEventCollider(bool enable) {
        if (enable)
        {
            _wasTriggeredOnce = false;
            _collider.center += _offset;
        }
        else
            _collider.center -= _offset;
    }
}
