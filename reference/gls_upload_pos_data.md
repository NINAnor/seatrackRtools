# Upload GLS position data to the SEATRACK database

Function to upload prepared GLS position data to the Sea Track database.

## Usage

``` r
gls_upload_pos_data(pos_list, chunk_size = 10)
```

## Arguments

- pos_list:

  A list of dataframes containing the prepared GLS position data, where
  each dataframe corresponds to a session and is named with the session
  ID.

- chunk_size:

  Integer specifying the number of sessions to upload in each chunk.
  Default is 10.

## Value

None. The function uploads the position data to the SEATRACK database.
