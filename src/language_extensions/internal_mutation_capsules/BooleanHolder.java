package language_extensions.internal_mutation_capsules;

/**
 * Created by jacob on 6/10/2014.
 */
public class BooleanHolder {
    private boolean theTruth;

    public BooleanHolder(boolean theTruth) {
        this.theTruth = theTruth;
    }

    public boolean getValue() {
        return theTruth;
    }

    public boolean setValue(boolean theTruth) {
        this.theTruth = theTruth;
        return this.theTruth;
    }
}
