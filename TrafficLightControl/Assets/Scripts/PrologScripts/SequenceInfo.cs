
public class SequenceInfo
{

    public enum JunctionSequence
    {
        unknown,
        sequence1,
        sequence2,
        sequence3,
        sequence4,
        sequence5,
        sequence6,
        sequence11,
        sequence12,
        sequence13,
        sequence14
    }

    public TrafficLight.Lights[] GreenLightes;
    public JunctionSequence Sequence;
    private int _duration;

    public int Duration
    {
        get { return _duration * 1000; }
        set { _duration = value; }
    }
}
