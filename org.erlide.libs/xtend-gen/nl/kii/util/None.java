package nl.kii.util;

import java.util.Iterator;
import java.util.LinkedList;
import nl.kii.util.NoneException;
import nl.kii.util.Opt;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;

@SuppressWarnings("all")
public class None<T extends Object> extends Opt<T> {
  @Override
  public T value() {
    try {
      throw new NoneException();
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Override
  public boolean hasSome() {
    return false;
  }
  
  @Override
  public boolean hasNone() {
    return true;
  }
  
  @Override
  public boolean hasError() {
    return false;
  }
  
  @Override
  public Iterator<T> iterator() {
    LinkedList<T> _newLinkedList = CollectionLiterals.<T>newLinkedList();
    return _newLinkedList.iterator();
  }
  
  @Override
  public boolean equals(final Object obj) {
    return (obj instanceof None<?>);
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
  
  @Override
  public String toString() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("None");
    return _builder.toString();
  }
}
