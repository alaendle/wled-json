namespace wled_json;

using LanguageExt;
using LanguageExt.Traits;

public class State<F> {
    public required K<F, bool> On { get; set; }
    public required K<F, int> Bri { get; set; }
    public required K<F, int> Transition { get; set; }
    public required K<F, int> Ps { get; set; }
    public required K<F, int> Pl { get; set; }
    public required K<F, int> Lor { get; set; }
    public required K<F, int> Mainseg { get; set; }

    public override string ToString()
    {
        return "State: On:" + On + " Bri:" + Bri + " Transition:"+ Transition + " Ps:"+ Ps + " Pl:"+ Pl + " Lor:"+ Lor + " Mainseg:" + Mainseg;
    }

    public override bool Equals(object? obj)
    {
        return obj is State<F> state &&
               On.Equals(state.On) &&
               Bri.Equals(state.Bri) &&
               Transition.Equals(state.Transition) &&
               Ps.Equals(state.Ps) &&
               Pl.Equals(state.Pl) &&
               Lor.Equals(state.Lor) &&
               Mainseg.Equals(state.Mainseg);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(On, Bri, Transition, Ps, Pl, Lor, Mainseg);
    }

    public bool ShouldSerializeOn()
    {
        return !this.On.Equals(Option<bool>.None);
    }

    public bool ShouldSerializeBri()
    {
        return !this.Bri.Equals(Option<int>.None);
    }

    public bool ShouldSerializeTransition()
    {
        return !this.Transition.Equals(Option<int>.None);
    }

    public bool ShouldSerializePs()
    {
        return !this.Ps.Equals(Option<int>.None);
    }

    public bool ShouldSerializePl()
    {
        return !this.Pl.Equals(Option<int>.None);
    }

    public bool ShouldSerializeLor()
    {
        return !this.Lor.Equals(Option<int>.None);
    }

    public bool ShouldSerializeMainseg()
    {
        return !this.Mainseg.Equals(Option<int>.None);
    }
}

public static class State {
        public static State<F> Empty<F>() where F : MonoidK<F> {
        return new State<F> {
            On = F.Empty<bool>(),
            Bri = F.Empty<int>(),
            Transition = F.Empty<int>(),
            Ps = F.Empty<int>(),
            Pl = F.Empty<int>(),
            Lor = F.Empty<int>(),
            Mainseg = F.Empty<int>()
        };

    }
}

public static class StateExtensions {
    public static State<F> Combine<F>(this State<F> state1, State<F> state2) where F : SemigroupK<F> {
        return new State<F> {
            On = state1.On.Combine(state2.On),
            Bri = state1.Bri.Combine(state2.Bri),
            Transition = state1.Transition.Combine(state2.Transition),
            Ps = state1.Ps.Combine(state2.Ps),
            Pl = state1.Pl.Combine(state2.Pl),
            Lor = state1.Lor.Combine(state2.Lor),
            Mainseg = state1.Mainseg.Combine(state2.Mainseg)
        };
    }
}