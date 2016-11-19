using UnityEngine;

public class BoomGate : TrafficLight {

    public new enum States
    {
        Open,
        Opening,
        Closed,
        Closing
    }

    public new States State;

    public GameObject pivot;

    private Vector3 pivotVector3;
    private Quaternion openPosition; 
    private Quaternion closePosition;

    private int degreeCounter = 0;

    private float multiplier = 1f;

    // Use this for initialization
    void Start () {
        State = States.Closed;
    }
	
	// Update is called once per frame
	void Update () {
        if(State == States.Closed || State == States.Open)
            return;

        rotatePivot();
	}

    private void rotatePivot()
    {
        //State opening -> opening barrier
        if (State == States.Opening)
        {
            pivot.transform.Rotate(0, 1*multiplier, 0);
            degreeCounter++;

            if (degreeCounter == 90)
            {
                degreeCounter = 0;
                State = States.Open;

                if (Collider != null)
                    EnableCollider(false);
            }
        } //State closing -> closing barrier
        else if (State == States.Closing)
        {
            pivot.transform.Rotate(0, -1*multiplier, 0);
            degreeCounter++;

            if (degreeCounter == 90)
            {
                degreeCounter = 0;
                State = States.Closed;

                if (Collider != null)
                    EnableCollider();
            }
        }
        //else if (State == States.Open && !open)
        //{
        //    pivot.transform.Rotate(-1, 0, 0);
        //    State = States.Closing;
        //}
    }

    public new void updateMultiplier(float value) {
        multiplier = value;
    }
}
