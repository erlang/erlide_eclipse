package org.erlide.util.event_tracer;


@SuppressWarnings("all")
public class NullEventHandler extends ErlideEventTracerHandler {
  public NullEventHandler() {
    super(null);
  }
  
  public void handle(final ErlideEvent event) {
  }
  
  public void dispose() {
  }
}
