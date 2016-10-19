using UnityEngine;
using System.Collections;

public class BoomGate : MonoBehaviour {

    public GameObject pivot;
    public bool isOpen = false;

    private int state;
    private enum States {
        open,
        opening,
        closed,
        closing
    }

    private Vector3 pivotVector3;
    private Quaternion openPosition; 
    private Quaternion closePosition;

    private int degreeCopunter = 0;

    // Use this for initialization
    void Start () {
        state = (int)States.closed;

    }
	
	// Update is called once per frame
	void Update () {
        rotatePivot(isOpen);
  
	}

    private void rotatePivot(bool open) {        
                
        if (state == (int)States.closed && open) {         

            state = (int)States.opening;
        }
        //State opening -> opening barrier
        else if(state == (int)States.opening && open) {
            pivot.transform.Rotate(0,1,0);

            degreeCopunter++;

            if (degreeCopunter == 90) {
                degreeCopunter = 0;

                state = (int)States.open;
            }
        }
        //State closing -> closing barrier
        else if (state == (int)States.closing && !open) {
            pivot.transform.Rotate(0, -1, 0);

            degreeCopunter++;

            if(degreeCopunter == 90) {
                degreeCopunter = 0;

                state = (int)States.closed;
            }
        }
        else if(state == (int)States.open && !open) {
            pivot.transform.Rotate(-1,0,0);
            state = (int)States.closing;
        }
    }
}
