using UnityEngine;
using Random = System.Random;

public class SplineWalker : MonoBehaviour
{
    public enum LookDirection
    {
        None,
        Forward,
        Backward,
        Left,
        Right,
        Up,
        Down
    }


    public enum SplineWalkerMode
    {
        Once,
        Loop,
        PingPong,
        Connected
    }


    public SplineWaypoint Waypoint;
    public float HeightOffset = 0.5f;
    public float duration;
    public LookDirection LookAtDirection;
    public SplineWalkerMode mode;

    private float progress;
    private BezierSpline spline;
    private bool isGoingForward = true;

    private Random rnd = new Random();

    private void Update()
    {

        spline = Waypoint.Spline;

        if (isGoingForward)
        {
            progress += Time.deltaTime / duration;
            if (progress > 1f)
            {
                switch (mode)
                {
                    case SplineWalkerMode.Once:
                        progress = 1f;
                        break;
                    case SplineWalkerMode.Loop:
                        progress -= 1f;
                        break;
                    case SplineWalkerMode.Connected:
                        if (Waypoint.NextWaypoint.IsDestination)
                        {
                            Destroy(gameObject);
                            return;
                        }

                        //Waypoint = Waypoint.NextWaypoint;
                        Waypoint = GetRandomWaypoint(Waypoint.NextWaypoint);

                        spline = Waypoint.Spline;
                        progress = 0;
                        break;
                    default:
                        progress = 2f - progress;
                        isGoingForward = false;
                        break;
                }
            }
        }
        else
        {
            progress -= Time.deltaTime / duration;
            if (progress < 0f)
            {
                progress = -progress;
                isGoingForward = true;
            }
        }


        Vector3 position = spline.GetPoint(progress) + Vector3.up * HeightOffset;
        transform.position = position;

        if(LookAtDirection == LookDirection.None)
            return;

        transform.LookAt(position + spline.GetDirection(progress));

        if(name.ToLower().Contains("truck"))
            transform.localEulerAngles += new Vector3(-90, 0, 0);

        switch (LookAtDirection)
        {
            case LookDirection.Forward:
                break;
            case LookDirection.Backward:
                transform.localEulerAngles += new Vector3(0, 180, 0);
                break;
            case LookDirection.Left:
                transform.localEulerAngles += new Vector3(0, 90, 0);
                break;
            case LookDirection.Right:
                transform.localEulerAngles += new Vector3(0, -90, 0);
                break;
            case LookDirection.Up:
                transform.localEulerAngles += new Vector3(0, 0, 90);
                break;
            case LookDirection.Down:
                transform.localEulerAngles += new Vector3(0, 0, -90);
                break;
        }
    }


    /// <summary>
    /// Get a random origin spawn point
    /// </summary>
    /// <returns></returns>
    private SplineWaypoint GetRandomWaypoint(SplineWaypoint waypoint)
    {
        var waypoints = waypoint.GetComponents<SplineWaypoint>();
        int rand = rnd.Next(0, waypoints.Length);

        return waypoints[rand];
    }
}