CLASS lsc_zdes_travel_i DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

ENDCLASS.

CLASS lsc_zdes_travel_i IMPLEMENTATION.

  METHOD save_modified.

  data : travel_log  type standard table of zdes_travel_log,
         travel_log_create type STANDARD TABLE OF zdes_travel_log,
         travel_log_update type STANDARD TABLE OF zdes_travel_log.

  if create-travel is not INITIAL.

  travel_log =  CORRESPONDING #( create-travel ).

  loop at travel_log ASSIGNING FIELD-SYMBOL(<lfs_travel_log>).

  <lfs_travel_log>-changing_operation = 'CREATE'.

  GET TIME STAMP FIELD <lfs_travel_log>-created_at.

  try.

  <lfs_travel_log>-change_id  = cl_system_uuid=>create_uuid_x16_static(  ).

  catch cx_uuid_error.

  ENDTRY.


  if create-travel[ 1 ]-%control-BookingFee = cl_abap_behv=>flag_changed.

  <lfs_travel_log>-changed_field_name = 'Booking fee'.
  <lfs_travel_log>-changed_value = create-travel[ 1 ]-BookingFee.

  <lfs_travel_log>-travelid = create-travel[ 1 ]-TravelId.
  ENDIF.


  if create-travel[ 1 ]-%control-AgencyId = cl_abap_behv=>flag_changed.

  <lfs_travel_log>-changed_field_name = 'Agency Id'.
  <lfs_travel_log>-changed_value = create-travel[ 1 ]-AgencyId.

  endif.


  APPEND <lfs_travel_log> to  travel_log_create.

  ENDLOOP.

  MODIFY zdes_travel_log from TABLE @travel_log_create.



  endif.

  if update-travel is not INITIAL.


  travel_log = CORRESPONDING #( update-travel ).

  LOOP at travel_log ASSIGNING FIELD-SYMBOL(<lfs_travel_update>).

   <lfs_travel_update>-changing_operation = 'UPDATE'.

   get TIME STAMP FIELD <lfs_travel_update>-created_at .

   try.

   <lfs_travel_update>-change_id = cl_system_uuid=>create_uuid_x16_static(  ).

   catch cx_uuid_error.


   ENDTRY.

   if update-travel[ 1 ]-%control-BookingFee = cl_abap_behv=>flag_changed.

  <lfs_travel_update>-changed_field_name = 'Booking fee'.
  <lfs_travel_update>-changed_value = update-travel[ 1 ]-BookingFee.
  <lfs_travel_update>-travelid = update-travel[ 1 ]-TravelId.

  ENDIF.

  if update-travel[ 1 ]-%control-AgencyId = cl_abap_behv=>flag_changed.

  <lfs_travel_update>-changed_field_name = 'Agency id'.
  <lfs_travel_update>-changed_value = update-travel[ 1 ]-AgencyId.
  <lfs_travel_update>-travelid = update-travel[ 1 ]-TravelId.

  ENDIF.

   APPEND <lfs_travel_update> TO travel_log_update.


  ENDLOOP.


  MODIFY zdes_travel_log FROM TABLE @travel_log_update.


  endif.


  if delete-travel is not INITIAL.

  endif.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_bookingsuppl DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS SetBookingSupplId FOR DETERMINE ON SAVE
      IMPORTING keys FOR BookingSuppl~SetBookingSupplId.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR BookingSuppl~calculateTotalPrice.

ENDCLASS.

CLASS lhc_bookingsuppl IMPLEMENTATION.

  METHOD SetBookingSupplId.


    DATA: max_bookingsupplid  TYPE /dmo/booking_supplement_id,
          bookingsuppliment   TYPE STRUCTURE FOR READ RESULT zdes_bksuppl_i,
          bookingsuppl_update TYPE TABLE FOR UPDATE zdes_travel_i\\BookingSuppl.


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
      ENTITY BookingSuppl BY \_Booking
      FIELDS ( BookingUuid )
      WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Booking BY \_BookingSupplement
    FIELDS ( BookingSupplementId )
    WITH CORRESPONDING #( bookings )
    LINK DATA(bookingsuppl_links)
    RESULT DATA(bookingsuppliments).




    LOOP AT bookings INTO DATA(booking).


      " initlaize the Booking ID number .
      max_bookingsupplid = '00'.

      LOOP AT bookingsuppl_links INTO DATA(bookingsuppl_link) USING KEY id WHERE source-%tky = booking-%tky.

        bookingsuppliment = bookingsuppliments[ KEY id
                            %tky = bookingsuppl_link-target-%tky  ].

        IF bookingsuppliment-BookingSupplementId > max_bookingsupplid.

          max_bookingsupplid = bookingsuppliment-BookingSupplementId.

        ENDIF.

      ENDLOOP.



      LOOP AT bookingsuppl_links INTO bookingsuppl_link USING KEY id WHERE source-%tky = booking-%tky.

        bookingsuppliment = bookingsuppliments[ KEY id
                            %tky = bookingsuppl_link-target-%tky  ].

        IF bookingsuppliment-BookingSupplementId IS INITIAL.

          max_bookingsupplid += 1.

          APPEND VALUE #( %tky = bookingsuppliment-%tky
                        BookingSupplementId = max_bookingsupplid
                               ) TO bookingsuppl_update.



        ENDIF.

      ENDLOOP.
    ENDLOOP.



    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY BookingSuppl
    UPDATE FIELDS ( BookingSupplementId )
    WITH bookingsuppl_update.






  ENDMETHOD.

  METHOD calculateTotalPrice.



    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
           ENTITY BookingSuppl BY \_Travel
           FIELDS ( TravelUuid )
           WITH CORRESPONDING #( keys )
           RESULT DATA(travels).

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
           ENTITY Travel
           EXECUTE reCalcTotalPrice
           FROM CORRESPONDING #( travels ).




  ENDMETHOD.




ENDCLASS.

CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS SetBookingDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~SetBookingDate.

    METHODS SetBookingId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~SetBookingId.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD SetBookingDate.
    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
           ENTITY Booking
           FIELDS ( BookingDate )
           WITH CORRESPONDING #( keys )
           RESULT DATA(Bookings).

    DELETE bookings WHERE BookingDate IS NOT INITIAL.

    CHECK bookings IS NOT INITIAL.

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).

      <booking>-BookingDate = cl_abap_context_info=>get_system_date( ).

    ENDLOOP.

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
           ENTITY Booking
           UPDATE FIELDS ( BookingDate )
           WITH CORRESPONDING #( Bookings ).
  ENDMETHOD.

  METHOD SetBookingId.


    DATA: max_bookingid   TYPE /dmo/booking_id,
          booking         TYPE STRUCTURE FOR READ RESULT zdes_booking_i,
          bookings_update TYPE TABLE  FOR UPDATE zdes_travel_i\\Booking.

    " We are reading Booking entiy to get the travel UUID field for the current Booking instance and
    " store that in Travels table.
    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Booking BY \_Travel
    FIELDS ( TravelUuid )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).


    " Now read all the Bookings related to travel which we got from top from the travels table.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Travel BY \_Booking
    FIELDS ( BookingId )
    WITH CORRESPONDING #( travels )
    LINK DATA(booking_links)
    RESULT DATA(bookings).




    LOOP AT travels INTO DATA(travel).


      " initlaize the Booking ID number .
      max_bookingid = '0000'.

      LOOP AT booking_links INTO DATA(booking_link) USING KEY id WHERE source-%tky = travel-%tky.

        booking = bookings[ KEY id
                            %tky = booking_link-target-%tky  ].

        IF booking-BookingId > max_bookingid.

          max_bookingid = booking-BookingId.

        ENDIF.

      ENDLOOP.



      LOOP AT booking_links INTO booking_link USING KEY id WHERE source-%tky = travel-%tky.

        booking = bookings[ KEY id
                            %tky = booking_link-target-%tky  ].


        IF booking-BookingId IS INITIAL.

          max_bookingid += 1.

          APPEND VALUE #( %tky = booking-%tky
                          BookingId = max_bookingid
                                 ) TO bookings_update.


        ENDIF.

      ENDLOOP.

    ENDLOOP.




    " Use Modify EML to update the Bookings entity with the new Booking id num  which is  max_bookingid

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Booking
    UPDATE FIELDS ( BookingId )
    WITH bookings_update.




  ENDMETHOD.

  METHOD calculateTotalPrice.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Booking BY \_travel
    FIELDS ( TravelUuid )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Travel
    EXECUTE reCalcTotalprice
    FROM CORRESPONDING #( travels ).



  ENDMETHOD.

ENDCLASS.

CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Travel RESULT result.
    METHODS setTravelId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelId.
    METHODS setOverallStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setOverallStatus.
    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.
    METHODS deductDiscount FOR MODIFY
      IMPORTING keys FOR ACTION Travel~deductDiscount RESULT result.
    METHODS GetDefaultsFordeductDiscount FOR READ
      IMPORTING keys FOR FUNCTION Travel~GetDefaultsFordeductDiscount RESULT result.
    METHODS reCalcTotalprice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~reCalcTotalprice.
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.
    METHODS validateCusomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCusomer.
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD setTravelId.

    "Read the entity Travel using EML
    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
         ENTITY Travel
         FIELDS ( TravelId )
         WITH CORRESPONDING #( keys )
         RESULT DATA(lt_travel).

    " Delete the Record where travel id is already existing
    DELETE lt_travel WHERE travelid IS NOT INITIAL.

    SELECT SINGLE FROM zdes_travel FIELDS MAX( travel_id ) INTO @DATA(lv_travelid_max).

    " Modify EML
    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
          ENTITY Travel
          UPDATE FIELDS ( TravelId )
          WITH VALUE #( FOR ls_travel_id IN lt_travel INDEX INTO lv_index
                           ( %tky = ls_travel_id-%tky
                             TravelId = lv_travelid_max + lv_index
                              ) ).

  ENDMETHOD.

  METHOD setOverallStatus.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
     ENTITY Travel
     FIELDS ( OverallStatus )
     WITH CORRESPONDING #( keys )
     RESULT DATA(lt_status).

    DELETE lt_status WHERE OverallStatus IS NOT INITIAL.

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Travel
    UPDATE FIELDS ( OverallStatus )
    WITH VALUE #(  FOR ls_status IN lt_status
                  (   %tky = ls_status-%tky
                      OverallStatus = 'O' ) ).








  ENDMETHOD.

  METHOD acceptTravel.
    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
        ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky = key-%tky
                                        OverallStatus = 'A' ) ).



    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
     ENTITY Travel
     ALL FIELDS WITH
     CORRESPONDING #( keys )
     RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky = travel-%tky
                                              %param = travel ) ).
  ENDMETHOD.

  METHOD rejectTravel.
    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
          ENTITY Travel
          UPDATE FIELDS ( OverallStatus )
          WITH VALUE #( FOR key IN keys ( %tky = key-%tky
                                          OverallStatus = 'R' ) ).



    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
     ENTITY Travel
     ALL FIELDS WITH
     CORRESPONDING #( keys )
     RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels ( %tky = travel-%tky
                                              %param = travel ) ).
  ENDMETHOD.

  METHOD deductDiscount.


    DATA : travel_for_update TYPE TABLE FOR UPDATE zdes_travel_i.

    DATA(keys_temp) = keys.


    LOOP AT keys_temp ASSIGNING FIELD-SYMBOL(<key_temp>) WHERE %param-discount_percent IS INITIAL OR
                                                                    %param-discount_percent > 100 OR
                                                                    %param-discount_percent < 0 .


      APPEND VALUE #( %tky = <key_temp>-%tky ) TO failed-travel.
      APPEND VALUE #( %tky = <key_temp>-%tky
                      %msg = new_message_with_text(  text = 'Invalid Discount percentage'
                                                     severity = if_abap_behv_message=>severity-error )
                      %element-totalprice  = if_abap_behv=>mk-on
                      %action-deductDiscount = if_abap_behv=>mk-on ) TO reported-travel.


      DELETE keys_temp .


    ENDLOOP.

    CHECK keys_temp IS NOT INITIAL.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Travel
    FIELDS ( TotalPrice )
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_travels).


    DATA :lv_percentage TYPE decfloat16.

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<fs_travel>).


      DATA(lv_discount_percent) = keys[ KEY id %tky = <fs_travel>-%tky ]-%param-discount_percent.

      lv_percentage = lv_discount_percent / 100.


      DATA(reduced_value) = <fs_travel>-TotalPrice * lv_percentage .

      reduced_value = <fs_travel>-TotalPrice - reduced_value.

      APPEND VALUE #( %tky = <fs_travel>-%tky

                       totalprice = reduced_value  ) TO travel_for_update .


    ENDLOOP.



    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY travel
    UPDATE FIELDS ( totalprice )
    WITH  travel_for_update .

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY travel
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(lt_travel_updated).



    result = VALUE #( FOR ls_travel IN lt_travel_updated  ( %tky = ls_travel-%tky
                                                            %param = ls_travel )  ).



  ENDMETHOD.

  METHOD GetDefaultsFordeductDiscount.


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
     ENTITY Travel
     FIELDS ( TotalPrice )
     WITH CORRESPONDING #( keys )
     RESULT DATA(travels).


    LOOP AT travels INTO DATA(travel).
      IF travel-TotalPrice >= 4000.
        APPEND VALUE #( %tky = travel-%tky
                        %param-discount_percent = 30 ) TO result.
      ELSE.
        APPEND VALUE #( %tky = travel-%tky
                           %param-discount_percent = 15 ) TO result.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD reCalcTotalprice.


    TYPES: BEGIN OF ty_amount_per_currencycode,

             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA : amounts_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY travel
    FIELDS ( bookingfee currencycode )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY travel BY \_booking
    FIELDS ( flightprice currencycode )
    WITH CORRESPONDING  #( travels )
    RESULT DATA(bookings)
    LINK DATA(booking_links).


    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY booking BY \_BookingSupplement
    FIELDS ( price currencycode )
    WITH CORRESPONDING #( bookings  )
    RESULT DATA(bookingsuppliments)
    LINK DATA(bookingsuppliments_links).




    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).

      amounts_per_currencycode =  VALUE #( ( amount = <travel>-BookingFee
                                           currency_code = <travel>-CurrencyCode )  ).


      LOOP AT booking_links INTO  DATA(booking_link) USING KEY id WHERE source-%tky = <travel>-%tky.

        DATA(booking) = bookings[ KEY id %tky = booking_link-target-%tky ].

        COLLECT VALUE ty_amount_per_currencycode( amount = booking-flightprice
                                                   currency_code = booking-currencycode ) INTO amounts_per_currencycode.

        LOOP AT  bookingsuppliments_links INTO DATA(bookingsuppliment_link)  USING KEY id  WHERE source-%tky = booking-%tky.

          DATA(bookingsupplement) = bookingsuppliments[ KEY id %tky = bookingsuppliment_link-target-%tky  ].


          COLLECT VALUE ty_amount_per_currencycode( amount = bookingsupplement-price
                                                     currency_code = bookingsupplement-currencycode ) INTO amounts_per_currencycode.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.


    DELETE amounts_per_currencycode WHERE currency_code IS INITIAL.

    LOOP AT amounts_per_currencycode INTO DATA(amount_per_currencycode).


      " Travel USD  - parent  - Total price
      "Booking EUR -> USD
      "Booking suppl EUR -> USD

      IF <travel>-CurrencyCode = amount_per_currencycode-currency_code .

        <travel>-TotalPrice += amount_per_currencycode-amount.

      ELSE.

        /dmo/cl_flight_amdp=>convert_currency(
                 EXPORTING
                   iv_amount                   =  amount_per_currencycode-amount
                   iv_currency_code_source     =  amount_per_currencycode-currency_code
                   iv_currency_code_target     =  <travel>-CurrencyCode
                   iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
                 IMPORTING
                   ev_amount                   = DATA(total_booking_price_per_curr)
                ).

        <travel>-TotalPrice += total_booking_price_per_curr.
      ENDIF.

      MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
      ENTITY Travel
      UPDATE FIELDS ( TotalPrice )
      WITH CORRESPONDING #( travels ).


    ENDLOOP.

  ENDMETHOD.

  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
    ENTITY Travel
    EXECUTE reCalcTotalprice
    FROM CORRESPONDING #( keys ).


  ENDMETHOD.

  METHOD validateCusomer.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
        ENTITY Travel
        FIELDS ( CustomerId )
        WITH CORRESPONDING #( keys )
        RESULT DATA(travels).



    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerId EXCEPT * ).

    SELECT FROM /dmo/customer FIELDS customer_id
    FOR ALL ENTRIES IN @customers
    WHERE customer_id = @customers-customer_id
    INTO TABLE @DATA(valid_customers).

    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #(  %tky                 = travel-%tky
                              %state_area          = 'VALIDATE_CUSTOMER'
                            ) TO reported-travel.

      IF travel-CustomerId IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerId ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
        %state_area         = 'VALIDATE_CUSTOMER'
                        %msg = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = |Not a Valid Customer  { travel-CustomerId }|
                                )
                        %element-CustomerId = if_abap_behv=>mk-on
                               ) TO reported-travel.



      ENDIF.


    ENDLOOP.


  ENDMETHOD.

  METHOD validateAgency.
    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
        ENTITY Travel
        FIELDS ( AgencyId )
        WITH CORRESPONDING #( keys )
        RESULT DATA(travels).



    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.
    agencies = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING agency_id = AgencyId EXCEPT * ).

    SELECT FROM /dmo/agency FIELDS agency_id
    FOR ALL ENTRIES IN @agencies
    WHERE agency_id = @agencies-agency_id
    INTO TABLE @DATA(valid_agencies).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky               = travel-%tky
                            %state_area        = 'VALIDATE_AGENCY'
                            ) TO reported-travel.
      IF travel-AgencyId IS NOT INITIAL AND NOT line_exists( valid_agencies[ agency_id = travel-AgencyId ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
          %state_area        = 'VALIDATE_AGENCY'
                        %msg = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = |Not a Valid Agency  { travel-AgencyId }|
                                )
                        %element-AgencyId = if_abap_behv=>mk-on
                               ) TO reported-travel.



      ENDIF.


    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
         ENTITY Travel
         FIELDS ( BeginDate EndDate )
         WITH CORRESPONDING #( keys )
         RESULT DATA(travels).


    LOOP AT travels INTO DATA(travel).
      APPEND VALUE #(  %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES' ) TO reported-travel.
      IF travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
          %state_area        = 'VALIDATE_DATES'
                        %msg = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = |Begin Date should not be blank|
                                )
                        %element-BeginDate = if_abap_behv=>mk-on
                               ) TO reported-travel.
      ENDIF.

      IF travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
          %state_area        = 'VALIDATE_DATES'
                        %msg = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = |End Date should not be blank|
                                )
                        %element-EndDate = if_abap_behv=>mk-on
                               ) TO reported-travel.
      ENDIF.

      IF travel-EndDate < travel-BeginDate AND  travel-BeginDate IS NOT INITIAL
                                           AND  travel-EndDate IS NOT INITIAL.

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
          %state_area        = 'VALIDATE_DATES'
                        %msg = new_message_with_text(
                                 severity = if_abap_behv_message=>severity-error
                                 text     = |End Date should not be less than Begin Date|
                                )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate = if_abap_behv=>mk-on
                               ) TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zdes_travel_i IN LOCAL MODE
           ENTITY Travel
           FIELDS ( OverallStatus )
           WITH CORRESPONDING #( keys )
           RESULT DATA(travels).


    result = VALUE #( FOR ls_travel IN travels
                       ( %tky = ls_travel-%tky
                         %field-BookingFee = COND #( WHEN ls_travel-OverallStatus = 'A'
                                                      THEN if_abap_behv=>fc-f-read_only
                                                      ELSE if_abap_behv=>fc-f-unrestricted )


                         %action-acceptTravel = COND #( WHEN ls_travel-OverallStatus = 'A'
                                                      THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled )

                         %action-rejectTravel = COND #( WHEN ls_travel-OverallStatus = 'R'
                                                      THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled )

                          %action-deductdiscount = COND #( WHEN ls_travel-OverallStatus = 'A'
                                                      THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled )
                          ) ).





  ENDMETHOD.

ENDCLASS.
