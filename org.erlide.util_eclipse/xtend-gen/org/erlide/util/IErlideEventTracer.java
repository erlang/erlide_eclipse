package org.erlide.util;

@SuppressWarnings("all")
public interface IErlideEventTracer {
  public abstract void traceSession();
  
  public abstract void traceReset();
  
  public abstract void traceCrash(final String backend);
  
  public abstract void traceStatus(final Object status);
  
  public abstract void traceOperation(final String operation, final long duration);
}
