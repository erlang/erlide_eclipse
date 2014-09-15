/* Copyright 2009-2012 Comcast Interactive Media, LLC.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */
package org.fishwife.jrugged;

import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Factory to create new {@link CircuitBreaker} instances and keep track of
 * them.
 */
public class CircuitBreakerFactory {

    public static final String CONFIG_KEY_PREFIX = "circuit";
    public static final String LIMIT_KEY = "limit";
    public static final String WINDOWMILLIS_KEY = "windowMillis";
    public static final String RESETMILLIS_KEY = "resetMillis";

    private final ConcurrentHashMap<String, CircuitBreaker> circuitBreakerMap = new ConcurrentHashMap<String, CircuitBreaker>();

    private Properties properties;

    private final Logger logger = java.util.logging.Logger.getLogger(getClass()
            .getName());

    /**
     * Create a new {@link CircuitBreaker} and map it to the provided name. If
     * the CircuitBreaker already exists, then the existing instance is
     * returned.
     *
     * @param name
     *            the name of the {@link CircuitBreaker}
     * @param config
     *            the {@link CircuitBreakerConfig} with the configuration
     *            values.
     * @return the created {@link CircuitBreaker}
     */
    public synchronized CircuitBreaker createCircuitBreaker(final String name,
            final CircuitBreakerConfig config) {
        CircuitBreaker circuitBreaker = findCircuitBreaker(name);

        if (circuitBreaker == null) {
            circuitBreaker = new CircuitBreaker(name);

            configureCircuitBreaker(name, circuitBreaker, config);
            addCircuitBreakerToMap(name, circuitBreaker);
        }

        return circuitBreaker;
    }

    /**
     * Set the {@link Properties} object to search for {@link CircuitBreaker}
     * property override values. The override values can be specified as:<br>
     * circuit.{circuit_name}.limit<br>
     * circuit.{circuit_name}.resetmillis<br>
     * circuit.{circuit_name}.windowmillis
     *
     * @param properties
     *            the {@link Properties} object to search.
     */
    public void setProperties(final Properties properties) {
        this.properties = properties;
    }

    /**
     * Find an existing {@link CircuitBreaker}
     *
     * @param name
     *            the value for the {@link CircuitBreaker}
     * @return the found {@link CircuitBreaker}, or null if it is not found.
     */
    public CircuitBreaker findCircuitBreaker(final String name) {
        return circuitBreakerMap.get(name);
    }

    /**
     * Get the {@link Set} of created {@link CircuitBreaker} names.
     *
     * @return the {@link Set} of names.
     */
    public Set<String> getCircuitBreakerNames() {
        return circuitBreakerMap.keySet();
    }

    protected void configureCircuitBreaker(final String name,
            final CircuitBreaker circuit, final CircuitBreakerConfig config) {

        long resetMillis = config.getResetMillis();
        final Long resetMillisOverride = getLongPropertyOverrideValue(name,
                RESETMILLIS_KEY);
        if (resetMillisOverride != null) {
            resetMillis = resetMillisOverride;
        }

        final FailureInterpreter fi = config.getFailureInterpreter();
        circuit.setFailureInterpreter(fi);

        if (resetMillis > 0) {
            circuit.setResetMillis(resetMillis);
        }

        if (fi instanceof DefaultFailureInterpreter) {
            configureDefaultFailureInterpreter(name, resetMillis, circuit);
        } else {
            logger.log(Level.INFO,
                    "Created CircuitBreaker '{}', resetMillis={}",
                    new Object[] { name, resetMillis });
        }
    }

    private void configureDefaultFailureInterpreter(final String name,
            final long resetMillis, final CircuitBreaker circuit) {
        final DefaultFailureInterpreter fi = (DefaultFailureInterpreter) circuit
                .getFailureInterpreter();

        final Integer limitOverride = getIntegerPropertyOverrideValue(name,
                LIMIT_KEY);

        if (limitOverride != null) {
            fi.setLimit(limitOverride);
        }

        final Long windowMillisOverride = getLongPropertyOverrideValue(name,
                WINDOWMILLIS_KEY);

        if (windowMillisOverride != null) {
            fi.setWindowMillis(windowMillisOverride);
        }

        logger.log(
                Level.INFO,
                "Created CircuitBreaker '{}', limit={}, windowMillis={}, resetMillis={}",
                new Object[] { name, fi.getLimit(), fi.getWindowMillis(),
                        resetMillis });
    }

    /**
     * Add a {@link CircuitBreaker} to the map.
     *
     * @param name
     *            the name for the {@link CircuitBreaker}
     * @param circuitBreaker
     *            the {@link CircuitBreaker} to add.
     */
    protected void addCircuitBreakerToMap(final String name,
            final CircuitBreaker circuitBreaker) {
        circuitBreakerMap.put(name, circuitBreaker);
    }

    /**
     * Get the property name for a circuit name and key.
     *
     * @param name
     *            the circuit name.
     * @param key
     *            the property key.
     * @return the property name.
     */
    private String getPropertyName(final String name, final String key) {
        return CONFIG_KEY_PREFIX + '.' + name + '.' + key;
    }

    /**
     * Get an integer property override value.
     *
     * @param name
     *            the {@link CircuitBreaker} name.
     * @param key
     *            the property override key.
     * @return the property override value, or null if it is not found.
     */
    private Integer getIntegerPropertyOverrideValue(final String name,
            final String key) {
        if (properties != null) {
            final String propertyName = getPropertyName(name, key);

            final String propertyOverrideValue = properties
                    .getProperty(propertyName);

            if (propertyOverrideValue != null) {
                try {
                    return Integer.parseInt(propertyOverrideValue);
                } catch (final NumberFormatException e) {
                    logger.log(
                            Level.SEVERE,
                            "Could not parse property override key={}, value={}",
                            new Object[] { key, propertyOverrideValue });
                }
            }
        }
        return null;
    }

    /**
     * Get an {@link Long} property override value.
     *
     * @param name
     *            the {@link CircuitBreaker} name.
     * @param key
     *            the property override key.
     * @return the property override value, or null if it is not found.
     */
    private Long getLongPropertyOverrideValue(final String name,
            final String key) {
        if (properties != null) {
            final String propertyName = getPropertyName(name, key);

            final String propertyOverrideValue = properties
                    .getProperty(propertyName);

            if (propertyOverrideValue != null) {
                try {
                    return Long.parseLong(propertyOverrideValue);
                } catch (final NumberFormatException e) {
                    logger.log(
                            Level.SEVERE,
                            "Could not parse property override key={}, value={}",
                            new Object[] { key, propertyOverrideValue });
                }
            }
        }
        return null;
    }
}
