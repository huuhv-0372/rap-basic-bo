CLASS zcl_generate_data_1999 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GENERATE_DATA_1999 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
*    MODIFY ENTITIES OF ZI_RAP_Travel_1999
*       ENTITY Travel
*       EXECUTE trigger_a
*       FROM VALUE #( ( TravelUUID = '7c4f791a-0552-4dca-bc52-560463f3fa64' ) ) "12345
*       MAPPED FINAL(mapped)
*       FAILED FINAL(failed)
*       REPORTED FINAL(reported).
*
*    COMMIT ENTITIES.
*
*    out->write( 'Travel restatus.' ).



*    xóa toàn bộ trong bản ghi trong table
    SELECT * FROM zrap_dtrav_1999 INTO TABLE @data(lt_data).
    DELETE zrap_dtrav_1999 FROM TABLE @lt_data.
    COMMIT WORK.




    " delete existing entries in the database table
* DELETE FROM zrap_atrav_1999.
* DELETE FROM zrap_abook_1999.
*
* " insert travel demo data
* INSERT zrap_atrav_1999 FROM (
*     SELECT
*       FROM /dmo/travel
*       FIELDS
*         uuid(  )      AS travel_uuid           ,
*         travel_id     AS travel_id             ,
*         agency_id     AS agency_id             ,
*         customer_id   AS customer_id           ,
*         begin_date    AS begin_date            ,
*         end_date      AS end_date              ,
*         booking_fee   AS booking_fee           ,
*         total_price   AS total_price           ,
*         currency_code AS currency_code         ,
*         description   AS description           ,
*         CASE status
*           WHEN 'B' THEN 'A' " accepted
*           WHEN 'X' THEN 'X' " cancelled
*           ELSE 'O'          " open
*         END           AS overall_status        ,
*         createdby     AS created_by            ,
*         createdat     AS created_at            ,
*         lastchangedby AS last_changed_by       ,
*         lastchangedat AS last_changed_at       ,
*         lastchangedat AS local_last_changed_at
*         ORDER BY travel_id UP TO 200 ROWS
*   ).
* COMMIT WORK.
*
* " insert booking demo data
* INSERT zrap_abook_1999 FROM (
*     SELECT
*       FROM   /dmo/booking    AS booking
*         JOIN zrap_atrav_1999 AS z
*         ON   booking~travel_id = z~travel_id
*       FIELDS
*         uuid( )                 AS booking_uuid          ,
*         z~travel_uuid           AS travel_uuid           ,
*         booking~booking_id      AS booking_id            ,
*         booking~booking_date    AS booking_date          ,
*         booking~customer_id     AS customer_id           ,
*         booking~carrier_id      AS carrier_id            ,
*         booking~connection_id   AS connection_id         ,
*         booking~flight_date     AS flight_date           ,
*         booking~flight_price    AS flight_price          ,
*         booking~currency_code   AS currency_code         ,
*         z~created_by            AS created_by            ,
*         z~last_changed_by       AS last_changed_by       ,
*         z~last_changed_at       AS local_last_changed_by
*   ).
* COMMIT WORK.
*
* out->write( 'Travel and booking demo data inserted.').
  ENDMETHOD.
ENDCLASS.
