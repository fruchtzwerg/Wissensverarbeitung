using UnityEngine;

public class Bezier : MonoBehaviour {

    /// <summary>
    /// Same as above only with directly calculating the curve instead of calling Lerp().
    /// 
    /// B(t) = (1 - t)3 P0 + 3 (1 - t)2 t P1 + 3 (1 - t) t2 P2 + t3 P3
    /// </summary>
    public static Vector3 GetPoint(Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3, float t)
    {
        t = Mathf.Clamp01(t);
        var oneMinusT = 1f - t;
        return
            oneMinusT * oneMinusT * oneMinusT * p0 +
            3f * oneMinusT * oneMinusT * t * p1 +
            3f * oneMinusT * t * t * p2 +
            t * t * t * p3;
    }

    /// <summary>
    /// Calculates the first derivative of a bezier curve.
    /// This can be used to represent velocity of an object
    /// moving along the curve.
    /// 
    /// B'(t) = 3 (1 - t)2 (P1 - P0) + 6 (1 - t) t (P2 - P1) + 3 t2 (P3 - P2).
    /// </summary>
    public static Vector3 GetVelocity(Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3, float t)
    {
        t = Mathf.Clamp01(t);
        var oneMinusT = 1f - t;
        var derivative = 3f * oneMinusT * oneMinusT * (p1 - p0) + 6f * oneMinusT * t * (p2 - p1) + 3f * t * t * (p3 - p2);

        return derivative;
    }
}
