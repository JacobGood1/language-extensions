package language_extensions.internal_mutation_capsules;

import clojure.lang.Ratio;

/**
 * Created by jacob on 6/10/2014.
 */
public class DoubleHolder {
    private double doubleValue;

    public DoubleHolder(double doubleValue) {
        this.doubleValue = doubleValue;
    }

    public double getValue() {
        return doubleValue;
    }

    public double setPlusValue(double doubleValue) {
        this.doubleValue += doubleValue;
        return this.doubleValue;
    }
    public double setPlusValue(long doubleValue) {
        this.doubleValue += doubleValue;
        return this.doubleValue;
    }
    public double setPlusValue(Ratio doubleValue) {
        this.doubleValue += doubleValue.doubleValue();
        return this.doubleValue;
    }

    public double setMinusValue(double doubleValue) {
        this.doubleValue -= doubleValue;
        return this.doubleValue;
    }
    public double setMinusValue(long doubleValue) {
        this.doubleValue -= doubleValue;
        return this.doubleValue;
    }
    public double setMinusValue(Ratio doubleValue) {
        this.doubleValue -= doubleValue.doubleValue();
        return this.doubleValue;
    }


    public double setTimesValue(double doubleValue) {
        this.doubleValue *= doubleValue;
        return this.doubleValue;
    }
    public double setTimesValue(long doubleValue) {
        this.doubleValue *= doubleValue;
        return this.doubleValue;
    }
    public double setTimesValue(Ratio doubleValue) {
        this.doubleValue *= doubleValue.doubleValue();
        return this.doubleValue;
    }

    public double setDividesValue(double doubleValue) {
        this.doubleValue = this.doubleValue / doubleValue;
        return this.doubleValue;
    }
    public double setDividesValue(long doubleValue) {
        this.doubleValue = this.doubleValue / doubleValue;
        return this.doubleValue;
    }
    public double setDividesValue(Ratio doubleValue) {
        this.doubleValue = this.doubleValue / doubleValue.doubleValue();
        return this.doubleValue;
    }

    public double setValue(double doubleValue){
        this.doubleValue = doubleValue;
        return doubleValue;
    }
    public double setValue(long doubleValue){
        this.doubleValue = doubleValue;
        return this.doubleValue;
    }
    public double setValue(Ratio doubleValue){
        this.doubleValue = doubleValue.doubleValue();
        return this.doubleValue;
    }
}
