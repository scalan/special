package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Type argument T should be reified in virtualized code by providing type descriptor Elem[T] */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Reified {
    //String value();
}
