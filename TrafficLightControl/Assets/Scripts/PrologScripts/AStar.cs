using UnityEngine;
using System.Collections;
using System.Text;
using System;

public class AStar : MonoBehaviour {

    private const string ARC = "arc(";
    private StringBuilder sb;
    public int multiplier = 10;

    [Serializable]
    public class AStarNode {
        public GameObject start;
        public GameObject[] followNodes;
    }
    public AStarNode[] nodeList;


	// Use this for initialization
	void Start () {
        sb = new StringBuilder();        
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    public string buildAStarTreeString() {
        //clear stringbuilder
        sb.Remove(0, sb.Length);

        sb.Append("[");

        createItems();
        sb.Remove(sb.Length - 1, 1);
        sb.Append("]");
        return sb.ToString();
    }

    private void createItems() {

        //each node
        foreach(var entry in nodeList) {

            //only if node has follow node(s)
            if (entry.followNodes.Length > 0) {
                //item foreach followNode
                foreach (var tmp in entry.followNodes) {
                    StringBuilder sb2 = new StringBuilder();

                    sb2.Append(ARC);
                    sb2.Append(entry.start.name.ToLower());
                    sb2.Append(",");
                    sb2.Append(tmp.name.ToLower());
                    sb2.Append(",");

                    /*
                     * TODO: Calculte third parameter!
                     */
                    sb2.Append(5);

                    sb2.Append(",");
                    sb2.Append(calculteDistance(entry.start, tmp));
                    sb2.Append("),");

                    sb.Append(sb2);                   
                }
            }
        }
    }

    private int calculteDistance(GameObject start, GameObject followNode) {

        float distance = Vector3.Distance(start.transform.position, followNode.transform.position) * multiplier;

        return (int)distance;
    }
}
