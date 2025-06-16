@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Suppl Consumption entity'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZDES_BKSUPPL_C as projection on ZDES_BKSUPPL_I
{
    key BooksupplUuid,
    TravelUUID,
    BookingUUID,
    BookingSupplementId,
    SupplementId,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    Price,
    CurrencyCode,
    LocalLastChangedAt,
    /* Associations */
    _Booking:redirected to parent ZDES_BOOKING_C,
    _Travel:redirected to  ZDES_TRAVEL_C
}
