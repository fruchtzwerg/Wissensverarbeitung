using UnityEngine;

public class CollisionDetection : MonoBehaviour
{

    public const string TAG_LIGHT = "Light";
    public const string TAG_VEHICLE = "Vehicle";
    public const string TAG_COL_FRONT = "Collider_Front";
    public const string TAG_COL_REAR = "Collider_Rear";
    
    private SplineWalker _walker;



    private void Start()
    {
        _walker = GetComponentInParent<SplineWalker>();
    }

    private void FixedUpdate()
    {
        _walker.Move = true;
    }



    private void OnTriggerStay(Collider other)
    {
        // halt if this front collider intersects either another rear or light collider
        if (CompareTag(TAG_COL_FRONT) && (other.CompareTag(TAG_COL_REAR) || other.CompareTag(TAG_LIGHT)))
        {
            _walker.Move = false;
        }

        if (CompareTag(TAG_COL_FRONT) && other.CompareTag(TAG_COL_FRONT))
        {
            var splineWalker = other.transform.parent.GetComponent<SplineWalker>();
            if (splineWalker != null)
            {
                splineWalker.DestroyOnNextUpdate = true;
                print(string.Format("{0} was destroyed because of frontal collision", other.transform.parent.name));
            }
        }

        var waypoint = other.GetComponent<SplineWaypoint>();
        if (waypoint == null)
            return;

        waypoint.IsOccupied = true;
    }



    private void OnTriggerExit(Collider other)
    {

        // rear collider exits spawn area
        if (CompareTag(TAG_COL_REAR))
        {
            var waypoint = other.GetComponent<SplineWaypoint>();
            if (waypoint == null)
                return;

            waypoint.IsOccupied = false;
        }
    }
}