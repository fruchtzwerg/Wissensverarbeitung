using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif


[ExecuteInEditMode]
public class SplineModifier : MonoBehaviour
{

    private BezierSpline[] _splines;

    void ShowSplines()
    {
        _splines = GetComponentsInChildren<BezierSpline>();

        foreach (var spline in _splines)
        {
            spline.UpdatePositionsInEditor();
        }
    }

#if UNITY_EDITOR
    // update in editor
    void OnEnable() { EditorApplication.update += ShowSplines; }
    void OnDisable() { EditorApplication.update -= ShowSplines; }
#endif
}
