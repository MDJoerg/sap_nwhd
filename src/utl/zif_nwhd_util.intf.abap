interface ZIF_NWHD_UTIL
  public .


  methods GET_ALL_MOD_COL
    returning
      value(RT_COL) type STRING_TABLE .

  methods GET_ALL_MOD_JOB
    returning
      value(RT_JOB) type STRING_TABLE .

  methods GET_ALL_MOD_PUB
    returning
      value(RT_PUB) type STRING_TABLE .

endinterface.
