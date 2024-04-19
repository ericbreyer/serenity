
/// A macro to dynamically log an event at runtime
#[macro_export]
macro_rules! dyn_event {
    ($lvl:ident, $($arg:tt)+) => {
        match $lvl {
            ::tracing::Level::TRACE => ::tracing::trace!($($arg)+),
            ::tracing::Level::DEBUG => ::tracing::debug!($($arg)+),
            ::tracing::Level::INFO => ::tracing::info!($($arg)+),
            ::tracing::Level::WARN => ::tracing::warn!($($arg)+),
            ::tracing::Level::ERROR => ::tracing::error!($($arg)+),
        }
    };
}

/// A macro to dynamically create a span at runtime
#[macro_export]
macro_rules! dyn_span {
    ($lvl:ident, $name:expr, $($arg:tt)+) => {
        match $lvl {
            ::tracing::Level::TRACE => ::tracing::trace_span!($name, $($arg)+),
            ::tracing::Level::DEBUG => ::tracing::debug_span!($name, $($arg)+),
            ::tracing::Level::INFO => ::tracing::info_span!($name, $($arg)+),
            ::tracing::Level::WARN => ::tracing::warn_span!($name, $($arg)+),
            ::tracing::Level::ERROR => ::tracing::error_span!($name, $($arg)+),
        }
    };
}