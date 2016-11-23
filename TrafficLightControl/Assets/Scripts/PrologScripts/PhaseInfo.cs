
public class PhaseInfo
{

    public enum JunctionPhase
    {
        unknown,
        phase1,
        phase2,
        phase3,
        phase4,
        phase5,
        phase6,
        phase11,
        phase12,
        phase13,
        phase14
    }

    public TrafficLight.Lights[] GreenLightes;
    public JunctionPhase Phase;
    private int _duration;

    public int Duration
    {
        get { return _duration * 1000; }
        set { _duration = value; }
    }
}
