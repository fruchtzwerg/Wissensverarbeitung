using UnityEngine;
using System.Collections;
using System;

public class TrainSpawner : MonoBehaviour {

    private GameObject trainLocomotive1;
    private GameObject trainLocomotive2;
    private GameObject trainWagon1;
    private GameObject trainWagon2;
    private GameObject trainSection1;
    private GameObject trainSection2;
    private GameObject trainSection3;

    public GameObject BERWayPoint;
    public GameObject HROWayPoint;

    public bool flag;
    public bool flagOld;

    private Timer timer;
    private int run = 0;

    public long timerInterval = 100;

    public enum TrainDirection {
        HRO,
        BER,
        Both
    }

    // Use this for initialization
    void Start () {
        trainLocomotive1 = Resources.Load("locomotive1") as GameObject;
        trainLocomotive2 = Resources.Load("locomotive2") as GameObject;
        trainWagon1 = Resources.Load("wagon1") as GameObject;
        trainWagon2 = Resources.Load("wagon2") as GameObject;
        trainSection1 = Resources.Load("sectionRubber") as GameObject;
        trainSection2 = Resources.Load("sectionRubber") as GameObject;
        trainSection3 = Resources.Load("sectionRubber") as GameObject;

        timer = new Timer();
        timer.Interval = timerInterval;
        timer.AutoReset = true;
        timer.Elapsed += timerElapsed;    
    }    

    // Update is called once per frame
    void Update () {
        if (flag != flagOld) {
            flagOld = flag;

            timer.Start();
        }

        timer.Update(Time.deltaTime);   
	}

    public void SpawnTrain(TrainDirection dir) {

        switch (dir){
            case TrainDirection.HRO:
                timer.Start();
                break;
            case TrainDirection.BER:
                timer.Start();
                break;
            default:
                break;
        }
    }

    private void buildTrain(Transform start, SplineWaypoint waypoint) {
        switch (run) {
            case 0:
                var l1 = Instantiate(trainLocomotive1, start) as GameObject;
                l1.transform.Rotate(0, 180, 0);
                var walkerL1 = l1.GetComponent<SplineWalker>();
                walkerL1.Waypoint = waypoint;
                walkerL1.Move = true;

                break;
            case 1:                
                var w1 = Instantiate(trainWagon1, start) as GameObject;
                var walkerW1 = w1.GetComponent<SplineWalker>();
                walkerW1.Waypoint = waypoint;
                walkerW1.Move = true;
                break;
            case 2:
                break;
            case 3:
                var w2 = Instantiate(trainWagon2, start) as GameObject;
                var walkerW2 = w2.GetComponent<SplineWalker>();
                walkerW2.Waypoint = waypoint;
                walkerW2.Move = true;

                break;
            case 4:
                var l2 = Instantiate(trainLocomotive2, start) as GameObject;
                var walkerL2 = l2.GetComponent<SplineWalker>();
                walkerL2.Waypoint = waypoint;
                walkerL2.Move = true;
                break;
            default:
                run = -1;
                timer.Stop();
                break;
        }
        /*
        var r1 = Instantiate(trainSection1, start) as GameObject;
        var walkerR1 = r1.GetComponent<SplineWalker>();
        walkerR1.Waypoint = waypoint;*/
    }
    

    private void timerElapsed(object sender, EventArgs e) {
        buildTrain(BERWayPoint.transform, BERWayPoint.GetComponent<SplineWaypoint>());
        run++;
    }
}
