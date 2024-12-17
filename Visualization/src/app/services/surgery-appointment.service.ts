import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { catchError, map, Observable, of, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { SurgeryAppointment } from '../domain/SurgeryAppointment';

@Injectable({
  providedIn: 'root'
})

export class SurgeryAppointmentService {
  theServerURL = environment.serverBaseUrl;
  token = localStorage.getItem('user') ? JSON.parse(localStorage.getItem('user')!).token : null;
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
      Authorization: `Bearer ${this.token}`
    })
  };

  constructor(private messageService: MessageService, private http: HttpClient) { }

  public createSurgeryAppointment(operationRequestId: string, roomNumber: string, startTime: string, endTime: string, startDate: string, endDate: string, staffList: string[]) {
    const url = `${this.theServerURL}/Appointment`;
    const surgAppointment: SurgeryAppointment = {
      operationRequestId: operationRequestId,
      roomNumber: roomNumber,
      startTime: startTime,
      endTime: endTime,
      startDate: startDate,
      endDate: endDate,
      staffList: staffList
    };

    this.http.post<SurgeryAppointment>(url, surgAppointment, this.httpOptions)
      .pipe(catchError(this.handleError<SurgeryAppointment>('Create surgery appointment')))
      .subscribe(data => {
        this.log(`Surgery Appointment was successfully created.`);
      });
  }

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if (error.status === 200 || error.status === 400) {
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured.`);
      }
      return of(result as T);
    };
  }
  private log(message: string) {
    this.messageService.add(`${message}`);
  }

}
