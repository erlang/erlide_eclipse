package org.erlide.util.event_tracer;

import org.erlide.util.event_tracer.ErlideEvent;
import org.erlide.util.event_tracer.ErlideEventTracerHandler;

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
