CLASS lhc_bookingsuppl DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS SetBookingSupplId FOR DETERMINE ON SAVE
      IMPORTING keys FOR BookingSuppl~SetBookingSupplId.

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

ENDCLASS.

CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS SetBookingDate FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~SetBookingDate.

    METHODS SetBookingId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Booking~SetBookingId.

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


DATA : travel_for_update type TABLE FOR UPDATE zdes_travel_i.

data(keys_temp) = keys.


LOOP at keys_temp ASSIGNING FIELD-SYMBOL(<key_temp>) where %param-discount_percent is Initial or
                                                                %param-discount_percent > 100 or
                                                                %param-discount_percent < 0 .


  APPEND VALUE #( %tky = <key_temp>-%tky ) to failed-travel.
  APPEND VALUE #( %tky = <key_temp>-%tky
                  %msg = new_message_with_text(  text = 'Invalid Discount percentage'
                                                 severity = if_abap_behv_message=>severity-error )
                  %element-totalprice  = if_abap_behv=>mk-on
                  %action-deductDiscount = if_abap_behv=>mk-on ) to reported-travel.


   delete keys_temp .


ENDLOOP.

CHECK keys_temp is NOT INITIAL.

READ ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY Travel
FIELDS ( TotalPrice )
WITH CORRESPONDING #( keys )
RESULT DATA(lt_travels).


data :lv_percentage type decfloat16.

LOOP at lt_travels ASSIGNING FIELD-SYMBOL(<fs_travel>).


DATA(lv_discount_percent) = keys[ key id %tky = <fs_travel>-%tky ]-%param-discount_percent.

lv_percentage = lv_discount_percent / 100.


data(reduced_value) = <fs_travel>-TotalPrice * lv_percentage .

reduced_value = <fs_travel>-TotalPrice - reduced_value.

APPEND VALUE #( %tky = <fs_travel>-%tky

                 totalprice = reduced_value  ) to travel_for_update .


ENDLOOP.



MODIFY ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY travel
UPDATE FIELDS ( totalprice )
with  travel_for_update .

READ ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY travel
ALL FIELDS WITH
CORRESPONDING #( keys )
RESULT data(lt_travel_updated).



result = value #( for ls_travel in lt_travel_updated  ( %tky = ls_travel-%tky
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



READ ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY travel
FIELDS ( bookingfee currencycode )
WITH CORRESPONDING #( keys )
RESULT data(travels).


read ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY travel by \_booking
FIELDS ( flightprice currencycode )
with CORRESPONDING  #( travels )
RESULT DATA(bookings)
link DATA(booking_links).


READ ENTITIES OF zdes_travel_i IN LOCAL MODE
ENTITY booking by \_BookingSupplement
FIELDS ( price currencycode )
with CORRESPONDING #( bookings  )
link data(bookingsuppliments_links).









  ENDMETHOD.

ENDCLASS.
