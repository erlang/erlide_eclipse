package nl.kii.util;

import com.google.common.base.Objects;
import java.util.Iterator;
import java.util.LinkedList;
import nl.kii.util.Opt;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function0;

@SuppressWarnings("all")
public class Some<T extends Object> extends Opt<T> implements Function0<T> {
  private T value;
  
  public Some(final T value) {
    if ((value == null)) {
      throw new NullPointerException("cannot create new Some(null)");
    }
    this.value = value;
  }
  
  @Override
  public T value() {
    return this.value;
  }
  
  @Override
  public T apply() {
    return this.value;
  }
  
  @Override
  public boolean hasSome() {
    return true;
  }
  
  @Override
  public boolean hasNone() {
    return false;
  }
  
  @Override
  public boolean hasError() {
    return false;
  }
  
  @Override
  public Iterator<T> iterator() {
    LinkedList<T> _newLinkedList = CollectionLiterals.<T>newLinkedList(this.value);
    return _newLinkedList.iterator();
  }
  
  @Override
  public int hashCode() {
    return this.value.hashCode();
  }
  
  @Override
  public boolean equals(final Object obj) {
    boolean _or = false;
    boolean _equals = Objects.equal(obj, this.value);
    if (_equals) {
      _or = true;
    } else {
      boolean _and = false;
      if (!(obj instanceof Some<?>)) {
        _and = false;
      } else {
        boolean _equals_1 = Objects.equal(((Some<?>) obj).value, this.value);
        _and = _equals_1;
      }
      _or = _and;
    }
    return _or;
  }
  
  @Override
  public String toString() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("Some(");
    _builder.append(this.value, "");
    _builder.append(")");
    return _builder.toString();
  }
}
