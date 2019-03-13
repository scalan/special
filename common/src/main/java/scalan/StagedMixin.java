package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

// has to be done in Java to be available at runtime

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface StagedMixin {
    String value();
}
