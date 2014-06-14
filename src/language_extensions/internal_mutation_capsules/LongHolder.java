package internal_mutation_capsules;

import clojure.lang.Ratio;

/**
 * Created by jacob on 6/8/2014.
 */
public class LongHolder {
    private long longValue;
    public LongHolder(long longValue) {
        this.longValue = longValue;
    }

    public long getValue() {
        return longValue;
    }

    public long setPlusValue(long longValue) {
        this.longValue += longValue;
        return this.longValue;
    }
    public long setMinusValue(long longValue) {
        this.longValue -= longValue;
        return this.longValue;
    }
    public long setTimesValue(long longValue) {
        this.longValue *= longValue;
        return this.longValue;
    }
    public long setDividesValue(long longValue) {
        this.longValue = this.longValue / longValue;
        return this.longValue;
    }

    public long setValue(long longValue){
        this.longValue = longValue;
        return this.longValue;
    }
}
