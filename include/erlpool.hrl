
-define(DEFAULT_MAX_PERIOD_SEC, 1).
-define(DEFAULT_MAX_INTENSITY, 100).
-define(DEFAULT_SUP_RESTART_STRATEGY, permanent).
-define(DEFAULT_SUP_SHUTDOWN, 2000).

-type pool_option()::
    {start_mfa, {module(), atom(), [term()]}} |
    {size, non_neg_integer()} |
    {supervisor_period, non_neg_integer()} |
    {supervisor_intensity, non_neg_integer()} |
    {supervisor_restart, supervisor:restart()} |
    {supervisor_shutdown, supervisor:shutdown()}.
