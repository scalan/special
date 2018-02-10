package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Mark the method as mutating the instance. */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
public @interface Updater {
   String methodName() default "";
}
