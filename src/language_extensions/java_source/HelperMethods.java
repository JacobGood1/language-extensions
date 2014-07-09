

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by jacob on 6/28/2014.
 */
public class HelperMethods
{
    //make sure that all objects use public values!
    public static String toStringFields(Object obj) throws ClassNotFoundException, IllegalAccessException {
        String finalOutPut = "";
        for(Field f: obj.getClass().getFields()){
            String fName = f.getName();
            Object fValue = f.get(obj);
            finalOutPut = finalOutPut + " " + fName + " = " + fValue + ", ";
        }
        return finalOutPut;
    }
}
