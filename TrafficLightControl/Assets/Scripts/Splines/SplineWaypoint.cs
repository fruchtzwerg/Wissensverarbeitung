using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif

[ExecuteInEditMode]
public class SplineWaypoint : MonoBehaviour
{

    public SplineWaypoint NextWaypoint;
    public BezierSpline Spline;

    public bool IsDestination;
    public bool IsOrigin;

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
