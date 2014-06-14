package internal_mutation_capsules;

import clojure.lang.Ratio;

import java.math.BigDecimal;

/**
 * Created by jacob on 6/10/2014.
 */
public class RatioHolder {
    private BigDecimal ratio;

    public RatioHolder(Ratio ratio) {
        this.ratio = ratio.decimalValue();
    }

    public BigDecimal getValue() {
        return ratio;
    }

    public BigDecimal setPlusValue(BigDecimal ratio) {
        this.ratio = this.ratio.add(ratio);
        return this.ratio;
    }
    public BigDecimal setPlusValue(Ratio ratio) {
        this.ratio = this.ratio.add(ratio.decimalValue());
        return this.ratio;
    }
    public BigDecimal setPlusValue(long ratio) {
        this.ratio = this.ratio.add(BigDecimal.valueOf(ratio));
        return this.ratio;
    }
    public BigDecimal setPlusValue(double ratio) {
        this.ratio = this.ratio.add(BigDecimal.valueOf(ratio));
        return this.ratio;
    }



    public BigDecimal setMinusValue(BigDecimal ratio) {
        this.ratio = this.ratio.add(ratio.negate());
        return this.ratio;
    }
    public BigDecimal setMinusValue(Ratio ratio) {
        this.ratio = this.ratio.add(ratio.decimalValue().negate());
        return this.ratio;
    }
    public BigDecimal setMinusValue(long ratio) {
        this.ratio = this.ratio.add(BigDecimal.valueOf(ratio).negate());
        return this.ratio;
    }
    public BigDecimal setMinusValue(double ratio) {
        this.ratio = this.ratio.add(BigDecimal.valueOf(ratio).negate());
        return this.ratio;
    }



    public BigDecimal setTimesValue(BigDecimal ratio) {
        this.ratio = this.ratio.multiply(ratio);
        return this.ratio;
    }
    public BigDecimal setTimesValue(Ratio ratio) {
        this.ratio = this.ratio.multiply(ratio.decimalValue());
        return this.ratio;
    }
    public BigDecimal setTimesValue(long ratio) {
        this.ratio = this.ratio.multiply(BigDecimal.valueOf(ratio));
        return this.ratio;
    }
    public BigDecimal setTimesValue(double ratio) {
        this.ratio = this.ratio.multiply(BigDecimal.valueOf(ratio));
        return this.ratio;
    }




    public BigDecimal setDividesValue(BigDecimal ratio) {
        this.ratio = this.ratio.divide(ratio);
        return this.ratio;
    }
    public BigDecimal setDividesValue(Ratio ratio) {
        this.ratio = this.ratio.divide(ratio.decimalValue());
        return this.ratio;
    }
    public BigDecimal setDividesValue(long ratio) {
        this.ratio = this.ratio.divide(BigDecimal.valueOf(ratio));
        return this.ratio;
    }
    public BigDecimal setDividesValue(double ratio) {
        this.ratio = this.ratio.divide(BigDecimal.valueOf(ratio));
        return this.ratio;
    }

    public BigDecimal setValue(double ratio){
        this.ratio = BigDecimal.valueOf(ratio);
        return this.ratio;
    }
    public BigDecimal setValue(long ratio){
        this.ratio = BigDecimal.valueOf(ratio);
        return this.ratio;
    }
    public BigDecimal setValue(Ratio ratio){
        this.ratio = ratio.decimalValue();
        return this.ratio;
    }

}
