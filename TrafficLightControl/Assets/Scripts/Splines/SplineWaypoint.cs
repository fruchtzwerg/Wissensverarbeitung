using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

[ExecuteInEditMode]
public class SplineWaypoint : MonoBehaviour
{

    public SplineWaypoint NextWaypoint;
    public BezierSpline Spline;
    public int Weight = 1;
    public int AStarCost = 1;
    public bool IsBusLane;

    public bool IsDestination;
    public bool IsOrigin;
    public bool IsOccupied { get; set; }

    private void CreateSpline()
    {
        if (NextWaypoint == null || Spline != null) return;

        Spline = gameObject.AddComponent<BezierSpline>();
        Spline.StartPoint = transform;
        Spline.EndPoint = NextWaypoint.transform;
    }

#if UNITY_EDITOR
    // update in editor
    void OnEnable() { EditorApplication.update += CreateSpline; }
    void OnDisable() { EditorApplication.update -= CreateSpline; }
#endif
}
