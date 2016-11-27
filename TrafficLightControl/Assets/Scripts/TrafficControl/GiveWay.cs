using System.Linq;
using UnityEngine;

public class GiveWay : MonoBehaviour
{

    public BoxCollider Stopper;
    public BezierSpline[] Lanes;
    private Vector3 _initPosition;
    private Vector3 _offsetPosition;

    private int _count;

    private void Start()
    {
        _initPosition = Stopper.transform.position;
        _offsetPosition = _initPosition + Vector3.down*100;

        Stopper.transform.position = _offsetPosition;
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.CompareTag(CollisionDetection.TAG_COL_REAR))
        {
            _count++;
        }
    }

    private void OnTriggerStay(Collider other)
    {
        var parent = other.transform.parent.parent;
        var isInLanes = Lanes.Any(lane => lane.transform == parent);
                       
        if (isInLanes && other.CompareTag(CollisionDetection.TAG_COL_REAR))
        {
            Stopper.transform.position = _initPosition;
        }
    }

    private void OnTriggerExit(Collider other)
    {
        if (other.CompareTag(CollisionDetection.TAG_COL_REAR))
        {
            _count--;

            if(_count <= 0)
                Stopper.transform.position = _offsetPosition;
        }
    }
}
