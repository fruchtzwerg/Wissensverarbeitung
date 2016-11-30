using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

public class UIMouseOver : MonoBehaviour {
    
    private GameObject _lanes;
    public static Transform SelectedOrigin;
    public static Transform SelectedDestination;

    private bool _isOrigin;
    //private Transform _selected;


    private void Start()
    {
        if (transform.parent.parent.name == "Origins")
            _isOrigin = true;
        if (transform.parent.parent.name == "Destinations")
            _isOrigin = false;

        _lanes = GameObject.Find(_isOrigin ? "SpawnLocations" : "DespawnLocations");
        //_selected = _isOrigin ? SelectedOrigin : SelectedDestination;
    }

    public void OnMouseEnter(BaseEventData e)
    {
        var marker = _lanes.transform.FindChild(name);

        if (_isOrigin)
        {
            if (SelectedOrigin.name == marker.name) return;
            SelectedOrigin.gameObject.SetActive(false);
        }
        else
        {
            if (SelectedDestination.name == marker.name) return;
            SelectedDestination.gameObject.SetActive(false);
        }
        marker.gameObject.SetActive(true);
    }

    public void OnMouseExit(BaseEventData e)
    {
        var marker = _lanes.transform.FindChild(name);

        if (_isOrigin)
        {
            if (SelectedOrigin.name == marker.name) return;
            SelectedOrigin.gameObject.SetActive(true);
        }
        else
        {
            if (SelectedDestination.name == marker.name) return;
            SelectedDestination.gameObject.SetActive(true);
        }
        marker.gameObject.SetActive(false);
    }

    public void OnClick(BaseEventData e)
    {
        var marker = _lanes.transform.FindChild(name);
        if (_isOrigin)
            SelectedOrigin = marker;
        else
            SelectedDestination = marker;
    }
}
