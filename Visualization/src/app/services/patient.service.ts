import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Patient } from '../domain/patient';
import { catchError, Observable, of } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class PatientService {
  theServerURL = 'https://localhost:5001/api/Patient';
  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(private messageService: MessageService, private http: HttpClient) { }

  CreatePatientProfile(firstName: string, lastName: string, phone: string, email: string, address: string, emergencyContact: string, gender: string, dateBirth: string) {
    const url = `${this.theServerURL}/Create-PatientProfile`;
    let patient: Patient = {
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      datebirth: dateBirth,
      emergencyContact: emergencyContact,
      gender: gender,
    };

    this.http.post<Patient>(url, patient, this.httpOptions)
      .pipe(catchError(this.handleError<Patient>('Create patient profile')))
      .subscribe(data => {
        this.log(`Patient profile: ${data.email} was successfully created.`);
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
      } else if(error.status === 200 ||error.status === 400 ){
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
