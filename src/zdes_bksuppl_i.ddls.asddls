@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Supplement Intf View'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZDES_BKSUPPL_I
  as select from zdes_bksuppl
  association     to parent ZDES_BOOKING_I as _Booking on $projection.BookingUUID = _Booking.BookingUuid

  association [1] to ZDES_TRAVEL_I         as _Travel  on $projection.TravelUUID = _Travel.TravelUuid
{
  key booksuppl_uuid        as BooksupplUuid,
      root_uuid             as TravelUUID,
      parent_uuid           as BookingUUID,
      booking_supplement_id as BookingSupplementId,
      supplement_id         as SupplementId,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,
      currency_code         as CurrencyCode,
      local_last_changed_at as LocalLastChangedAt,
      _Booking, // Make association public
      _Travel
}
