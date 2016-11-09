using System.Collections.Generic;
using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif


[ExecuteInEditMode]
public class Bezier3D : MonoBehaviour
{
    public Vector3 start = new Vector3(0, 0, 0);
    public Vector3 End = new Vector3(1, 1, 0);
    public Vector3 Handle1 = new Vector3(0, 1, 0);
    public Vector3 Handle2 = new Vector3(1, 0, 0);
    public int Resolution = 12;
    public float Thickness = 0.25f;

    public BezierSpline Spline;

    public void Start()
    {
        if (!GetComponent<MeshFilter>())
            gameObject.AddComponent<MeshFilter>();
        if (!GetComponent<MeshRenderer>())
            gameObject.AddComponent<MeshRenderer>();
        GetComponent<MeshFilter>().mesh = CreateMesh();
    }

    //cacluates point coordinates on a quadratic curve
    private static Vector3 PointOnPath(float t, Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3)
    {
        Vector3 p;

        var u = 1 - t;
        var uu = u*u;
        var uuu = uu*u;

        var tt = t*t;
        var ttt = tt*t;

        p = uuu*p0;
        p += 3*uu*t*p1;
        p += 3*u*tt*p2;
        p += ttt*p3;

        return p;
    }

    private Mesh CreateMesh()
    {
        Mesh mesh;

        mesh = new Mesh();

        const float scaling = 1;
        var width = Thickness/2f;
        var vertList = new List<Vector3>();
        var triList = new List<int>();
        var uvList = new List<Vector2>();
        var upNormal = new Vector3(0, 0, -1);

        triList.AddRange(new[]
        {
            2, 1, 0, //start face
            0, 3, 2
        });

        for (var s = 0; s < Resolution; s++)
        {
            var t = ((float) s)/Resolution;
            var futureT = ((float) s + 1)/Resolution;

            Vector3 segmentStart;
            Vector3 segmentEnd;
            if (!Spline)
            {
                segmentStart = PointOnPath(t, start, Handle1, Handle2, End);
                segmentEnd = PointOnPath(futureT, start, Handle1, Handle2, End);
            }
            else
            {
                segmentStart = Spline.GetPoint(t);
                segmentEnd = Spline.GetPoint(futureT);
            }

            var segmentDirection = segmentEnd - segmentStart;
            if (s == 0 || s == Resolution - 1)
                segmentDirection = new Vector3(0, 1, 0);
            segmentDirection.Normalize();
            var segmentRight = Vector3.Cross(upNormal, segmentDirection);
            segmentRight *= width;
            var offset = segmentRight.normalized*(width/2)*scaling;
            var br = segmentRight + upNormal*width + offset;
            var tr = segmentRight + upNormal*-width + offset;
            var bl = -segmentRight + upNormal*width + offset;
            var tl = -segmentRight + upNormal*-width + offset;

            var curTriIdx = vertList.Count;

            Vector3[] segmentVerts =
            {
                segmentStart + br,
                segmentStart + bl,
                segmentStart + tl,
                segmentStart + tr
            };
            vertList.AddRange(segmentVerts);

            Vector2[] uvs =
            {
                new Vector2(1, 0),
                new Vector2(0, 0),
                new Vector2(0, 1),
                new Vector2(1, 1)
            };
            uvList.AddRange(uvs);

            int[] segmentTriangles =
            {
                curTriIdx + 6, curTriIdx + 5, curTriIdx + 1, //left face
                curTriIdx + 1, curTriIdx + 2, curTriIdx + 6,
                curTriIdx + 7, curTriIdx + 3, curTriIdx + 0, //right face
                curTriIdx + 0, curTriIdx + 4, curTriIdx + 7,
                curTriIdx + 1, curTriIdx + 5, curTriIdx + 4, //top face
                curTriIdx + 4, curTriIdx + 0, curTriIdx + 1,
                curTriIdx + 3, curTriIdx + 7, curTriIdx + 6, //bottom face
                curTriIdx + 6, curTriIdx + 2, curTriIdx + 3
            };
            triList.AddRange(segmentTriangles);

            // final segment fenceposting: finish segment and add end face
            if (s == Resolution - 1)
            {
                curTriIdx = vertList.Count;

                vertList.AddRange(new[]
                {
                    segmentEnd + br,
                    segmentEnd + bl,
                    segmentEnd + tl,
                    segmentEnd + tr
                });

                uvList.AddRange(new[]
                {
                    new Vector2(0, 0),
                    new Vector2(0, 1),
                    new Vector2(1, 1),
                    new Vector2(1, 1)
                }
                    );
                triList.AddRange(new[]
                {
                    curTriIdx + 0, curTriIdx + 1, curTriIdx + 2, //end face
                    curTriIdx + 2, curTriIdx + 3, curTriIdx + 0
                });
            }
        }

        mesh.vertices = vertList.ToArray();
        mesh.triangles = triList.ToArray();
        mesh.uv = uvList.ToArray();
        mesh.RecalculateNormals();
        mesh.RecalculateBounds();
        mesh.Optimize();

        return mesh;
    }

    // run Start() in scene edtor every frame
#if UNITY_EDITOR
    void OnEnable() { EditorApplication.update += Start; }
    void OnDisable() { EditorApplication.update -= Start; }
#endif
}