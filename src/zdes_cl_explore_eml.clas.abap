CLASS zdes_cl_explore_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS simple_form_eml_create.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zdes_cl_explore_eml IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*-> EML CREATE

*-> Case 1 Creating Entity for Travel CDS only

* The %cid value will automatically assign the UUID for us. Any name can be given here, such as Dummy1

    MODIFY ENTITY zdes_travel_i
    CREATE
    SET FIELDS WITH VALUE #( ( %cid          = 'Dummy1'
                               AgencyId      = '70028'
                               Description   = 'New Agency 70028'
                               BookingFee    = 20
                               TotalPrice    = 3000
                               OverallStatus = 'A'
                               CurrencyCode  = 'USD'
                               BeginDate     = cl_abap_context_info=>get_system_date(  )
                               EndDate       = cl_abap_context_info=>get_system_date(  ) + 3 ) )
    FAILED DATA(lt_create_failed)
    REPORTED DATA(lt_create_reported).

    COMMIT ENTITIES
    RESPONSE OF zdes_travel_i
    FAILED DATA(lt_commit_failed1)
    REPORTED DATA(lt_commit_reported1).

    out->write( 'Case 1: New travel record created' ).

*-> Case 2 Creating Entity for Booking CDS with Travel

*-> Let's give mandatory fields for creation with Fields

    MODIFY ENTITY zdes_travel_i
    CREATE
    FIELDS ( AgencyId Description BookingFee TotalPrice CurrencyCode BeginDate EndDate )
    WITH VALUE #( ( %cid = 'Dummy2'
                     AgencyId      = '70028'
                     Description   = 'New Agency 70028 v2'
                     BookingFee    = 23
                     TotalPrice    = 3500
                     OverallStatus = 'A'
                     CurrencyCode  = 'USD'
                     BeginDate     = cl_abap_context_info=>get_system_date(  )
                     EndDate       = cl_abap_context_info=>get_system_date(  ) + 3 ) )
    CREATE BY \_Booking FIELDS ( CarrierId FlightDate CustomerId ConnectionId )
* Here %cid_ref will be the GUID that will be created for the above trip, so that the connection between the two CDS will be established with this GUID.
    WITH VALUE #( ( %cid_ref = 'Dummy2'
                    %target = VALUE #( ( %cid = 'Dummy_booking'
                                         CarrierId = '33'
                                         FlightDate = cl_abap_context_info=>get_system_date( )
                                         CustomerId = '11'
                                         ConnectionId = '22' ) ) ) )
    FAILED FINAL(lt_create_failed2)
    REPORTED FINAL(lt_create_reported2)
    MAPPED FINAL(lt_create_mapped2).


    COMMIT ENTITIES
    RESPONSE OF zdes_travel_i
    FAILED DATA(lt_commit_failed2)
    REPORTED DATA(lt_commit_reported2).

    out->write( 'Case 2: New travel and reservation/booking records created' ).


*-> Case 2 v2

*-> With the example below, 2 travel records and 2 reservation records for the first trip and 1 reservation record for the second trip are created.

    MODIFY ENTITY zdes_travel_i
    CREATE
    FIELDS ( AgencyId Description BookingFee TotalPrice CurrencyCode BeginDate EndDate )
    WITH VALUE #( ( %cid = 'Dummy3'
                    AgencyId      = '70028'
                    Description   = 'New Agency 70028 v3'
                    BookingFee    = 16
                    TotalPrice    = 4500
                    OverallStatus = 'O'
                    CurrencyCode  = 'USD'
                    BeginDate     = cl_abap_context_info=>get_system_date(  )
                    EndDate       = cl_abap_context_info=>get_system_date(  ) + 3 )

                  ( %cid = 'Dummy4'
                    AgencyId      = '70028'
                    Description   = 'New Agency 70028 v4'
                    BookingFee    = 23
                    TotalPrice    = 3500
                    OverallStatus = 'X'
                    CurrencyCode  = 'USD'
                    BeginDate     = cl_abap_context_info=>get_system_date(  )
                    EndDate       = cl_abap_context_info=>get_system_date(  ) + 3 ) )

    CREATE BY \_Booking FIELDS ( CarrierId FlightDate CustomerId ConnectionId )
    WITH VALUE #( ( %cid_ref = 'Dummy3'
                    %target = VALUE #( ( %cid = 'Dummy3_booking1'
                                         CarrierId = '333'
                                         CustomerId = '32'
                                         ConnectionId = '989'
                                         FlightDate = cl_abap_context_info=>get_system_date(  ) )

                                       ( %cid = 'Dummy3_booking2'
                                        CarrierId = '334'
                                        CustomerId = '33'
                                        ConnectionId = '990'
                                        FlightDate = cl_abap_context_info=>get_system_date(  ) ) ) )

                  ( %cid_ref = 'Dummy4'
                    %target = VALUE #( ( %cid = 'Dummy4_booking1'
                                         CarrierId = '335'
                                         CustomerId = '34'
                                         ConnectionId = '991'
                                         FlightDate = cl_abap_context_info=>get_system_date(  ) ) ) ) )
    FAILED FINAL(lt_create_fail3)
    REPORTED FINAL(lt_create_reported3)
    MAPPED FINAL(lt_create_mapped3).

    COMMIT ENTITIES
    RESPONSE OF zdes_travel_i
    FAILED DATA(lt_commit_failed3)
    REPORTED DATA(lt_commit_reported3).

    out->write( 'Case2 v2: Trips and reservations for these trips have been created!' ).


*-> EML Update

    DATA: lt_update TYPE TABLE FOR UPDATE zdes_travel_i.

    lt_update = VALUE #( ( TravelUuid = '22145873AFD91FE093A3022426BA72A9' CurrencyCode = 'USD'
       Description = 'EML Update Operation' BookingFee = 50 ) ).

    MODIFY ENTITIES OF zdes_travel_i
    ENTITY Travel
    UPDATE "SET FIELDS
    FIELDS ( CurrencyCode Description BookingFee )
    WITH lt_update
    FAILED DATA(lt_failed_up)
    REPORTED DATA(lt_reported_up).

    COMMIT ENTITIES.

    out->write( 'Entity Updated Successfully' ).


*-> EML Delete

    MODIFY ENTITIES OF zdes_travel_i
    ENTITY Travel
    DELETE
    FROM VALUE #( ( TravelUuid = '22145873AFD91FE093A3022426BA72A9' ) )
    FAILED DATA(lt_failed)
    REPORTED DATA(lt_reported).

    COMMIT ENTITIES
    RESPONSE OF zdes_travel_i
    FAILED DATA(lt_commit_failed)
    REPORTED DATA(lt_commit_reported).


*-> EML Read

*-> Case 1 Since there is no FIELDS, only the TravelUuid column is returned, the others will be empty.

    READ ENTITIES OF zdes_travel_i
    ENTITY Travel
    FROM VALUE #( ( TravelUuid = '22145873AFD91FE093A3349C47B132A9' ) )
    RESULT DATA(lt_case1).

    out->write( 'READ operation without FIELDS keyword' ).

*-> Since Case 2 is FIELDS, only the 2 field data we specified and the TravelUuid are brought.

    READ ENTITIES OF zdes_travel_i
    ENTITY Travel
    FIELDS ( TravelId AgencyId )
    WITH VALUE #( ( TravelUuid = '22145873AFD91FE093A3349C47B132A9' ) )
    RESULT DATA(lt_case2).

    out->write( 'READ operation with FIELDS keyword' ).

*-> Case 3 ALL FIELDS If we want to bring all fields

    READ ENTITIES OF zdes_travel_i
    ENTITY Travel
    ALL FIELDS
    WITH VALUE #( ( TravelUuid = '22145873AFD91FE093A3349C47B132A9' ) )
    RESULT DATA(lt_case3).

    out->write( 'READ operation with the ALL FIELDS keyword' ).

*-> Case 4 Association using READ (since _Booking association is used, the reservation data for that trip is fetched)

    READ ENTITIES OF zdes_travel_i
    ENTITY Travel
    BY \_Booking
    ALL FIELDS
    WITH VALUE #( ( TravelUuid = '22145873AFD91FE093A3349C47B132A9' ) )
    RESULT DATA(lt_case4).

    out->write( 'READ operation with ASSOCIATION' ).

*-> Case 5 If an attempt is made to read with an invalid GUID (Only lt_failed is filled in)

    READ ENTITIES OF zdes_travel_i
    ENTITY Travel
    ALL FIELDS
    WITH VALUE #( ( TravelUuid = '11111111111111111111111111111111' ) )
    RESULT DATA(lt_case5)
    FAILED DATA(lt_failed5)
    REPORTED DATA(lt_reported5).

    out->write( 'Trying to read a record that does not exist' ).





  ENDMETHOD.


  METHOD simple_form_eml_create.
*-> EML Create
    "Declaration of data objects using BDEF derived types

    DATA: cr_tab        TYPE TABLE FOR CREATE zdes_travel_i,    "input derived type
          cr_booking    TYPE TABLE FOR CREATE zdes_travel_i\_Booking,
          mapped_resp   TYPE RESPONSE FOR MAPPED zdes_travel_i, "response parameters
          failed_resp   TYPE RESPONSE FOR FAILED zdes_travel_i,
          reported_resp TYPE RESPONSE FOR REPORTED zdes_travel_i.



    cr_tab = VALUE #(
            ( %cid   = 'cid1'
                TravelId = 1
                AgencyId = '12345'
                CustomerId    = '123'
                BookingFee = 690
                TotalPrice = 690
                CurrencyCode = 'USD'
                Description = 'EML Create Travel ID 1' )
            ( %cid = 'cid2'
              "Just to demo %data/%key. You can specify fields with or without
              "the derived type components
              %data = VALUE #( TravelId = 2
                               AgencyId = '67893'
                               CustomerId    = '456'
                               BookingFee = 980
                               TotalPrice = 690
                               CurrencyCode = 'EUR'
                               Description = 'EML Create Travel ID 2' ) ) ).


    "EML statement, short form
    "root_ent must be the full name of the root entity, it is basically the name of the BDEF

    MODIFY ENTITY zdes_travel_i
      CREATE "determines the kind of operation
      FIELDS ( AgencyId CustomerId BookingFee CurrencyCode Description ) WITH cr_tab   "Fields to be respected for the
                                                       "input derived type and the input
                                                       "derived type itself
      MAPPED mapped_resp          "mapping information
      FAILED failed_resp          "information on failures with instances
      REPORTED reported_resp.     "messages


    COMMIT ENTITIES.
  ENDMETHOD.

ENDCLASS.
