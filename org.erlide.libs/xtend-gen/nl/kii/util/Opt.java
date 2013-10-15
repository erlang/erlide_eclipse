package nl.kii.util;

import nl.kii.util.NoneException;

@SuppressWarnings("all")
public abstract class Opt<T extends Object> implements Iterable<T> {
  public abstract T value() throws NoneException;
  
  public abstract boolean hasSome();
  
  public abstract boolean hasNone();
  
  public abstract boolean hasError();
}
