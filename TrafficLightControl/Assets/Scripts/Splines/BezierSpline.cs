using System;
using UnityEditor;
using UnityEngine;

[ExecuteInEditMode]
public class BezierSpline : MonoBehaviour
{
    
    [SerializeField]
    private Vector3[] points;
    [SerializeField]
    private BezierControlPointMode[] modes;
    [SerializeField]
    private bool loop;
    [SerializeField]
    private Transform startPoint;
    [SerializeField]
    private Transform endPoint;

    public int CurveCount
    {
        get { return (points.Length - 1)/3; }
    }

    public Transform StartPoint
    {
        get { return startPoint; }
        set
        {
            startPoint = value;
            if (value != null)
            {
                //points[0] = value.localPosition;
                SetControlPoint(0, value.localPosition);
            }
        }
    }

    public Transform EndPoint
    {
        get { return endPoint; }
        set
        {
            endPoint = value;
            if (value != null)
            {
                //points[points.Length - 1] = value.localPosition;
                SetControlPoint(points.Length -1, value.localPosition);
            }
        }
    }

    public int ControlPointCount
    {
        get { return points.Length; }
    }

    public bool Loop
    {
        get { return loop; }
        set
        {
            loop = value;
            if (value == true)
            {
                modes[modes.Length - 1] = modes[0];
                SetControlPoint(0, points[0]);
            }
        }
    }

    internal Quaternion GetRotation(float t)
    {
        return Quaternion.LookRotation(GetDirection(t), Vector3.up);
    }


    void Reset()
    {
        Vector3 startVector;
        Vector3 endVector;

        // set start point to defined start
        if (StartPoint != null)
            startVector = StartPoint.localPosition;
        else
            startVector = new Vector3(6f, 1f, 1f);

        // set endpooint to defined end
        if (StartPoint != null)
            endVector = EndPoint.localPosition;
        else
            endVector = new Vector3(24f, 1f, 1f);


        points = new Vector3[]
        {
            startVector,
            new Vector3(12f, 1f, 1f),
            new Vector3(18f, 1f, 1f),
            endVector
        };
        modes = new BezierControlPointMode[] {
            BezierControlPointMode.Free,
            BezierControlPointMode.Free
        };
    }

    public Vector3 GetControlPoint(int index)
    {
        return points[index];
    }

    public void SetControlPoint(int index, Vector3 point)
    {
        if (index % 3 == 0)
        {
            Vector3 delta = point - points[index];
            if (loop)
            {
                if (index == 0)
                {
                    points[1] += delta;
                    points[points.Length - 2] += delta;
                    points[points.Length - 1] = point;
                }
                else if (index == points.Length - 1)
                {
                    points[0] = point;
                    points[1] += delta;
                    points[index - 1] += delta;
                }
                else
                {
                    points[index - 1] += delta;
                    points[index + 1] += delta;
                }
            }
            else
            {
                if (index > 0)
                {
                    points[index - 1] += delta;
                }
                if (index + 1 < points.Length)
                {
                    points[index + 1] += delta;
                }
            }
        }
        points[index] = point;
        EnforceMode(index);
    }

    public BezierControlPointMode GetControlPointMode(int index)
    {
        return modes[(index + 1) / 3];
    }

    public void SetControlPointMode(int index, BezierControlPointMode mode)
    {
        int modeIndex = (index + 1) / 3;
        modes[modeIndex] = mode;
        if (loop)
        {
            if (modeIndex == 0)
            {
                modes[modes.Length - 1] = mode;
            }
            else if (modeIndex == modes.Length - 1)
            {
                modes[0] = mode;
            }
        }
        EnforceMode(index);
    }

    private void EnforceMode(int index)
    {
        int modeIndex = (index + 1) / 3;
        BezierControlPointMode mode = modes[modeIndex];
        if (mode == BezierControlPointMode.Free || !loop && (modeIndex == 0 || modeIndex == modes.Length - 1))
        {
            return;
        }

        int middleIndex = modeIndex * 3;
        int fixedIndex, enforcedIndex;
        if (index <= middleIndex)
        {
            fixedIndex = middleIndex - 1;
            if (fixedIndex < 0)
            {
                fixedIndex = points.Length - 2;
            }
            enforcedIndex = middleIndex + 1;
            if (enforcedIndex >= points.Length)
            {
                enforcedIndex = 1;
            }
        }
        else
        {
            fixedIndex = middleIndex + 1;
            if (fixedIndex >= points.Length)
            {
                fixedIndex = 1;
            }
            enforcedIndex = middleIndex - 1;
            if (enforcedIndex < 0)
            {
                enforcedIndex = points.Length - 2;
            }
        }

        Vector3 middle = points[middleIndex];
        Vector3 enforcedTangent = middle - points[fixedIndex];
        if (mode == BezierControlPointMode.Aligned)
        {
            enforcedTangent = enforcedTangent.normalized * Vector3.Distance(middle, points[enforcedIndex]);
        }
        points[enforcedIndex] = middle + enforcedTangent;
    }



    public Vector3 GetPoint(float t)
    {
        int i;
        if (t >= 1f)
        {
            t = 1f;
            i = points.Length - 4;
        }
        else
        {
            t = Mathf.Clamp01(t)*CurveCount;
            i = (int) t;
            t -= i;
            i *= 3;
        }
        return transform.TransformPoint(Bezier.GetPoint(
            points[i], points[i + 1], points[i + 2], points[i + 3], t));
    }


    public Vector3 GetVelocity(float t)
    {
        int i;
        if (t >= 1f)
        {
            t = 1f;
            i = points.Length - 4;
        }
        else
        {
            t = Mathf.Clamp01(t)*CurveCount;
            i = (int) t;
            t -= i;
            i *= 3;
        }
        return (Bezier.GetVelocity(
            points[i], points[i + 1], points[i + 2], points[i + 3], t));
    }


    public Vector3 GetDirection(float t)
    {
        return GetVelocity(t).normalized;
    }


    public void AddCurve()
    {
        Vector3 point = points[points.Length - 1];
        Array.Resize(ref points, points.Length + 3);
        point.x += 1f;
        points[points.Length - 3] = point;
        point.x += 1f;
        points[points.Length - 2] = point;
        point.x += 1f;
        points[points.Length - 1] = point;

        Array.Resize(ref modes, modes.Length + 1);
        modes[modes.Length - 1] = modes[modes.Length - 2];
        EnforceMode(points.Length - 4);

        if (loop)
        {
            points[points.Length - 1] = points[0];
            modes[modes.Length - 1] = modes[0];
            EnforceMode(0);
        }
    }


    public void UpdatePositionsInEditor()
    {
        if (startPoint != null)
            SetControlPoint(0, Vector3.zero);
        if (endPoint != null)
            SetControlPoint(points.Length - 1, endPoint.localPosition - startPoint.localPosition);
    }

    // update in editor
    void OnEnable() { EditorApplication.update += UpdatePositionsInEditor; }
    void OnDisable() { EditorApplication.update -= UpdatePositionsInEditor; }
}

public enum BezierControlPointMode
{
    Free,
    Aligned,
    Mirrored
}
