-author("silviu.caragea").

-define(DEFAULT_MAX_PERIOD_SEC, 1).
-define(DEFAULT_MAX_INTENSITY, 100).

-type pool_option()::
    {start_mfa, {module(), atom(), [term()]}} |
    {size, non_neg_integer()} |
    {supervisor_period, non_neg_integer()} |
    {supervisor_intensity, non_neg_integer()}.