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

ENDCLASS.
